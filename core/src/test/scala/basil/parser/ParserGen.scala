package basil.parser
import basil.data._
import cats.free.FreeApplicative
import cats.{Applicative, ~>}
import org.json4s.{JArray, JBool, JDouble, JNull, JObject, JString, JValue}
import org.scalacheck.Gen

trait ParserGen {
  type OpTree[I] = HFix[ParseOps, I]

  def opsJsGen(depth: Int): Gen[(OpTree[_], JValue, Any)] = {
    for {
      ops                 <- opsGen(depth)
      (jValue, extracted) <- genJs(ops)
    } yield (ops, jValue, extracted)
  }

  def jsArrGen[A](n: Int, gen: ObjExpectedGen[A]): ObjExpectedGen[A] = {
    for {
      list               <- Gen.listOfN(n + 1, randomJsGen)
      (target, finalVal) <- gen
    } yield {
      val (a, b) = list.splitAt(n)
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
  private def endingOpGen: Gen[ExpectedTerminator => OpTree[_]] = {
    val s: ExpectedTerminator => OpTree[String]  = _ => HFix[ParseOps, String](GetString)
    val n: ExpectedTerminator => OpTree[Double]  = x => HFix[ParseOps, Double](GetNum(x))
    val b: ExpectedTerminator => OpTree[Boolean] = _ => HFix[ParseOps, Boolean](GetBool)

    Gen.oneOf(s, n, b)
  }

  implicit class GenOps[A](gen: Gen[A]) {
    def branch: Gen[(A, A)] = gen.map(s => s -> s)
  }

  private def nonEndingOpGen[_](
      gen: Gen[ExpectedTerminator => OpTree[_]]): Gen[ExpectedTerminator => OpTree[_]] = {

    for {
      endingOp <- gen
      n        <- Gen.choose(0, 5)
      key      <- Gen.alphaStr.filter(_.nonEmpty)
      op <- Gen.oneOf[OpTree[_]](
             HFix(GetN(n, endingOp(OneOf(Bracket, Comma)))),
             HFix(GetKey(key, endingOp(OneOf(Comma, CurlyBrace)))),
             HFix(GetOpt(endingOp(Comma)).asInstanceOf[ParseOps[OpTree, _]]).asInstanceOf[OpTree[_]]
           )
    } yield { _: ExpectedTerminator =>
      op
    }
  }

  private def opsGen(depth: Int): Gen[OpTree[_]] = {
    def recurse(depth: Int): Gen[ExpectedTerminator => OpTree[_]] = {
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

  implicit val objExpectedGen: Applicative[ObjExpectedGen] = new Applicative[ObjExpectedGen] {
    override def pure[A](x: A): ObjExpectedGen[A] = Gen.const(JNull -> x)
    override def ap[A, B](ff: ObjExpectedGen[A => B])(fa: ObjExpectedGen[A]): ObjExpectedGen[B] = {
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
      } else obj -> Some(a)
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
          case GetMultiple(all)  => productJSGen(all)

          case getOpt: GetOpt[ObjExpectedGen, a] => optionGen(getOpt.next)
        }
      }
    }

  private val specialChar = Gen.oneOf("\\b", "\\r", "\\f", "\\\\", "\\/")

  private def productJSGen[A](
      all: FreeApplicative[ParseOps[ObjExpectedGen, ?], A]): ObjExpectedGen[A] = {
    all.foldMap(gen)
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
