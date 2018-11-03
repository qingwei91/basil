package basil.parser

import basil.data._
import cats.Functor
import cats.syntax.functor._
import org.json4s._
import org.json4s.native.JsonMethods.{compact, render}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, WordSpec}
import schemes.Fix
import schemes._

abstract class ParseSpec[F[_]: Functor]
    extends WordSpec
    with MustMatchers
    with GeneratorDrivenPropertyChecks
    with ParseOpsConstructor
    with ParserGen {

  implicit val parser: JsonParse[F, JValue]
  def liftF(charArr: Array[Char]): F[Char]
  def getLast[A](f: F[A]): A

  private implicit class FOps[A](f: F[(JValue, A)]) {
    def getJVal: JValue = getLast(f.map(_._1))
  }

  "stream parser" should {
    "parse string" in new PContext[F] {
      import parser._
      forAll(jstrGen) { js =>
        val jsonStr = compact(render(js)).toCharArray
        val decoded = parseString(path)(liftF(jsonStr)).getJVal

        decoded mustBe js
      }
    }

    "parse boolean" in new PContext[F] {
      import parser._
      List(JBool.True, JBool.False).foreach { js =>
        val jsonStr = compact(render(js)).toCharArray
        val decoded = parseBoolean(path)(liftF(jsonStr)).getJVal

        decoded mustBe js
      }
    }
    "parse number" in new PContext[F] {
      import parser._
      forAll(jnumGen) { num =>
        val jsonStr = compact(render(num)).toCharArray
        val decoded = parseNumber(End)(path)(liftF(jsonStr)).getJVal
        decoded mustBe num
      }
    }

    "parse array" in new PContext[F] {
      import parser._
      forAll(jsArrGen(12, Gen.oneOf(jstrGen, jstrGen))) { js =>
        val js = JArray(List.fill(12)(JString("")))
        val i  = (js.arr.size / 2)

        val jsonStr = compact(render(js)).toCharArray

        val decoded =
          parseArrayItem(i, parser.parseString)(path)(liftF(jsonStr)).getJVal

        decoded mustBe a[JString]
      }
    }
    "parse array specific" in new PContext[F] {
      import parser._
      val jsonStr = compact(render(JArray(List(JString(""))))).toCharArray

      val decoded =
        parseArrayItem(0, parseString)(path)(liftF(jsonStr)).getJVal

      decoded mustBe a[JString]
    }
    "parse object" in new PContext[F] {
      forAll(jsObjGen("myKey", jsArrGen(2, jstrGen))) { obj =>
        val jsonStr = compact(render(obj)).toCharArray

        val ops     = Start.getKey("myKey").getN(1).getString.t
        val decoded = parseJSStream(ops, liftF(jsonStr)).getJVal
        decoded mustBe a[JString]
      }
    }

    "testbed" in new PContext[F] {
      val ops = Start
        .getN(1)
        .getKey("mfxteosecEmnzqdfGftarkgojYpxkd")
        .getN(9)
        .getBool

      val obj = JArray(
        List(
          JObject(
            List(
              ("mfxteosecEmnzqdfGftarkgojYpxkd",
               JArray(
                 List(JBool(false),
                      JBool(false),
                      JBool(false),
                      JBool(false),
                      JBool(true),
                      JBool(false),
                      JBool(true),
                      JBool(false),
                      JBool(true),
                      JBool(true)))))),
          JObject(
            List(
              ("mfxteosecEmnzqdfGftarkgojYpxkd",
               JArray(
                 List(JBool(false),
                      JBool(false),
                      JBool(false),
                      JBool(false),
                      JBool(false),
                      JBool(false),
                      JBool(false),
                      JBool(true),
                      JBool(true),
                      JBool(true))))))
        ))

      val str      = compact(render(obj)).toCharArray
      val decoded1 = parseJSStream(ops.t, liftF(str)).getJVal

      decoded1 mustBe a[JBool]
    }

    "work" in new PContext[F] {
      forAll(opsJsGen(5)) {
        case (ops, js) =>
          val jsStr = compact(render(js)).toCharArray

          val decoded = parseJSStream(Fix(ops), liftF(jsStr)).getJVal

          decoded mustBe a[JValue]
      }
    }
  }
}

private abstract class PContext[F[_]](implicit val parser: JsonParse[F, JValue]) {
  implicit val path: Vector[PPath] = Vector.empty

  def parseJSStream(ops: Fix[ParseOps], s: F[Char]): F[(JValue, F[Char])] =
    Parser.parseJS[F, JValue](ops, s)(parser)
}

trait ParserGen {
  private def endingOpGen: Gen[ExpectedTerminator => ParseOps[Nothing]] = {
    val s: ExpectedTerminator => ParseOps[Nothing] = _ => GetString
    val n: ExpectedTerminator => ParseOps[Nothing] = x => GetNum(x)
    val b: ExpectedTerminator => ParseOps[Nothing] = _ => GetBool

    Gen.oneOf(s, n, b)
  }

  type TransformOps = ParseOps[Fix[ParseOps]] => ParseOps[Fix[ParseOps]]

  private def nonEndingOpGen(gen: Gen[ExpectedTerminator => ParseOps[Fix[ParseOps]]])
    : Gen[ExpectedTerminator => ParseOps[Fix[ParseOps]]] = {

    val nullableG: Gen[TransformOps] = {
      Gen
        .frequency(
          1 -> Gen.const(true),
          9 -> Gen.const(false)
        )
        .map { isNull =>
          if (isNull) { ops =>
            GetNullable(Fix(ops))
          } else { ops =>
            ops
          }
        }
    }
    for {
      endingOp <- gen
      n        <- Gen.choose(0, 5)
      key      <- Gen.alphaStr.filter(_.nonEmpty)
      nullable <- nullableG
      op <- Gen.oneOf(GetN(n, Fix(endingOp(OneOf(Bracket, Comma)))),
                      GetKey(key, Fix(endingOp(OneOf(Comma, CurlyBrace)))))
    } yield { _: ExpectedTerminator =>
      nullable(op)
    }
  }

  def opsGen(depth: Int): Gen[ParseOps[Fix[ParseOps]]] = {
    def recurse(depth: Int): Gen[ExpectedTerminator => ParseOps[Fix[ParseOps]]] = {
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

  def opsJsGen(depth: Int): Gen[(ParseOps[Fix[ParseOps]], JValue)] = {
    for {
      ops    <- opsGen(depth)
      jValue <- jsGen(ops)
    } yield ops -> jValue
  }

  def jsGen(op: ParseOps[Fix[ParseOps]]): Gen[JValue] = {
    op match {
      case GetString         => jstrGen
      case GetNum(_)         => jnumGen
      case GetBool           => Gen.oneOf(JBool.False, JBool.True)
      case GetNullable(ops)  => Gen.some(jsGen(ops.unfix)).map(_.fold[JValue](JNull)(identity))
      case GetN(n, next)     => jsArrGen(n + 1, jsGen(next.unfix))
      case GetKey(key, next) => jsObjGen(key, jsGen(next.unfix))
    }
  }

  // todo: handle escape char, omg
  val jstrGen = for {
    s <- Gen.alphaStr.suchThat(_.length < 200)
  } yield {
    JString(s)
  }

  val jnumGen = for {
    n <- Gen.choose(Double.MinValue, Double.MaxValue)
  } yield {
    JDouble(n)
  }

  def jsObjGen(key: String, gen: Gen[JValue]): Gen[JObject] = {
    for {
      v <- gen
    } yield {
      JObject(key -> v)
    }
  }

  val jsonGen = for {
    x <- Gen.oneOf(jstrGen, jnumGen)
  } yield {
    x
  }

  def jsArrGen(size: Int, gen: Gen[JValue]) = {
    for {
      js <- Gen.listOfN(size, gen)
    } yield {
      JArray(js)
    }
  }
}
