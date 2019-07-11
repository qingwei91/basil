package basil
package parser

import basil.data._
import basil.typeclass.Lazy
import cats.arrow.FunctionK
import cats.data.NonEmptyMap
import cats.free.FreeApplicative
import cats.{Applicative, ~>}
import org.json4s.{JArray, JBool, JDouble, JNull, JObject, JString, JValue}
import org.scalacheck.Gen

trait ParserGen {

  def opsJsGen(depth: Int): Gen[(ParseTree[_], JValue, Any)] = {
    for {
      ops                 <- opsGen(depth)
      (jValue, extracted) <- genJs(ops)
    } yield (ops, jValue, extracted)
  }

  def jsArrGen[A](size: Int, gen: ObjExpectedGen[A]): ObjExpectedGen[A] = {
    for {
      list               <- Gen.listOfN(size + 1, randomJsGen)
      (target, finalVal) <- gen
    } yield {
      val (a, b) = list.splitAt(size)
      JArray(a ::: (target :: b)) -> finalVal
    }
  }

  val jstrGen =
    for {
      raw <- Gen.asciiPrintableStr.suchThat(_.length < 200)
      sc  <- specialChar
    } yield {
      // todo: have special test case for escape char
      // we need to replace as escape char need to followed by control sequence
      // which requires a different generator
      JString(raw.replace("\\", sc))
    }

  val jnumGen =
    for {
      n <- Gen.choose(Double.MinValue, Double.MaxValue)
    } yield {
      JDouble(n)
    }

  val jBoolGen = for {
    b <- Gen.oneOf(true, false)
  } yield JBool(b)

  def jsObjGen[A](key: String, gen: ObjExpectedGen[A]): ObjExpectedGen[A] = {
    for {
      n                   <- Gen.choose(2, 8)
      obj                 <- randomObj(n)
      (nested, drillDown) <- gen
    } yield {
      val merged = (key -> nested) :: obj.obj
      JObject(merged) -> drillDown
    }
  }

  private def endingOpGen: Gen[ExpectedTerminator => ParseTree[_]] = {
    val s: ExpectedTerminator => ParseTree[String]  = _ => HFix[ParseOps, String](GetString)
    val n: ExpectedTerminator => ParseTree[Double]  = x => HFix[ParseOps, Double](GetNum(x))
    val b: ExpectedTerminator => ParseTree[Boolean] = _ => HFix[ParseOps, Boolean](GetBool)

    Gen.oneOf(s, n, b)
  }

  private def nonEndingOpGen(
      nextGen: Gen[ExpectedTerminator => ParseTree[_]]): Gen[ExpectedTerminator => ParseTree[_]] = {

    def optFallBack(nextOp: ExpectedTerminator => ParseTree[_]) = {
      val x = GetOpt[ParseTree, Any](nextOp(ExpectedTerminator.all).asInstanceOf[ParseTree[Any]])
      HFix(x).asInstanceOf[ParseTree[_]]
    }

    for {
      nextOp <- nextGen
      n      <- Gen.choose(0, 5)
      key    <- Gen.alphaStr.filter(_.nonEmpty)
      op <- Gen.oneOf[ParseTree[_]](
             HFix(GetN(n, nextOp(ExpectedTerminator.arrayTerm))),
             HFix(GetKey(key, nextOp(ExpectedTerminator.objTerm))),
             optFallBack(nextOp)
           )
    } yield { _: ExpectedTerminator =>
      op
    }
  }

  private def opsGen(depth: Int): Gen[ParseTree[_]] = {
    def recurse(depth: Int): Gen[ExpectedTerminator => ParseTree[_]] = {
      depth match {
        case 0 => endingOpGen
        case n => nonEndingOpGen(recurse(n - 1))
      }
    }
    if (depth == 0) {
      endingOpGen.map(f => f(End))
    } else {
      recurse(depth).map(f => f(End))
    }
  }

  /**
    * Generate a pair of value,
    * Full JsObject and value to be extracted
    */
  type ObjExpectedGen[E] = Gen[(JValue, E)]

  implicit val objExpectedGenApplicative: Applicative[ObjExpectedGen] =
    new Applicative[ObjExpectedGen] {
      override def pure[A](x: A): ObjExpectedGen[A] = Gen.const(JNull -> x)
      override def ap[A, B](ff: ObjExpectedGen[A => B])(
          fa: ObjExpectedGen[A]): ObjExpectedGen[B] = {
        for {
          x      <- fa
          (_, a) = x
          y      <- ff
          (o, f) = y
        } yield {
          o -> f(a)
        }
      }
    }

  private def genJs[I](expr: HFix[ParseOps, I]): ObjExpectedGen[I] =
    HFix.cata[ParseOps, I, ObjExpectedGen](expr, gen)

  private val boolGen = Gen.oneOf(JBool.False, JBool.True)

  private def optionGen[A](inner: ObjExpectedGen[A]): ObjExpectedGen[Option[A]] = {
    for {
      optional <- Gen.oneOf(true, false)
      (obj, a) <- inner
    } yield {
      if (optional) {
        JNull -> None
      } else {
        a match {
          case None => JNull -> None // nested None should be flatten
          case _    => obj   -> Some(a)
        }
      }
    }
  }

  private val gen: ParseOps[ObjExpectedGen, ?] ~> ObjExpectedGen =
    new (ParseOps[ObjExpectedGen, ?] ~> ObjExpectedGen) {
      override def apply[A](fa: ParseOps[ObjExpectedGen, A]): ObjExpectedGen[A] = {
        fa match {
          case GetString         => jstrGen.map(x => x -> x.values)
          case GetNum(_)         => jnumGen.map(x => x -> x.values)
          case GetBool           => boolGen.map(x => x -> x.values)
          case GetN(n, next)     => jsArrGen(n, next)
          case GetKey(key, next) => jsObjGen(key, next)
          case GetProduct(all)   => all.foldMap(FunctionK.id)
          case GetSum(oneOf)     => oneOfJSGen(oneOf)

          case getOpt: GetOpt[ObjExpectedGen, a] => optionGen(getOpt.next)
          case mapped: Mapped[ObjExpectedGen, h, A] =>
            mapped.fi.map {
              case (js, h) => js -> mapped.fn(h)
            }
        }
      }
    }

  private val specialChar = Gen.oneOf("\\b", "\\r", "\\f", "\\\\", "\\/")

  def productJSGen[A](all: FreeApplicative[ParseOps[ObjExpectedGen, ?], A]): ObjExpectedGen[A] = {
    all.foldMap(gen)
  }

  private def oneOfJSGen[A](
      oneOf: NonEmptyMap[String, Lazy[ObjExpectedGen[A]]]): ObjExpectedGen[A] = {
    val subtypes = oneOf.toSortedMap.values.toList
    val size     = subtypes.length

    if (size >= 2) {
      val rest = subtypes.tail.map(_.value)
      Gen.oneOf(subtypes.head.value, rest.head, rest.tail: _*)
    } else {
      subtypes.head.value
    }
  }

  private def randomPair = {
    for {
      key   <- jstrGen.map(_.values).suchThat(s => s.length < 200)
      value <- randomJsGen
    } yield {
      key -> value
    }
  }

  private def randomObj(n: Int) = {
    Gen.listOfN(n, randomPair).map { kvs =>
      JObject(kvs)
    }
  }

  private def randomJsGen: Gen[JValue] = {

    Gen.frequency(
      6 -> jstrGen,
      6 -> jnumGen,
      6 -> jBoolGen,
      1 -> Gen.listOfN(5, Gen.lzy(randomJsGen)).map(ls => JArray(ls)),
      1 -> Gen.choose[Int](2, 12).flatMap(randomObj)
    )
  }
}
