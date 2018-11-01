package basil.parser

import fs2.Stream
import schemes._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.scalacheck._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import cats.effect.IO

import scala.concurrent.Await
import scala.concurrent.duration._

class ParserSpec
    extends WordSpec
    with MustMatchers
    with GeneratorDrivenPropertyChecks
    with ParseOpsConstructor
    with ParserGen {

  "new parser" should {
    "parse string" in new Context {
      import jsonParse._
      forAll(jstrGen) { js =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded =
          parseString(path)(Stream.apply(jsonStr: _*)).compile.lastOrError

        decoded.get._1 mustBe js
      }
    }

    "parse boolean" in new Context {
      import jsonParse._
      List(JBool.True, JBool.False).foreach { js =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded = parseBoolean(path)(Stream.apply(jsonStr: _*)).compile.lastOrError

        decoded.get._1 mustBe js
      }
    }
    "parse number" in new Context {
      import jsonParse._
      forAll(jnumGen) { num =>
        val jsonStr = compact(render(num)).toCharArray
        val decoded =
          parseNumber(End)(path)(Stream.apply(jsonStr: _*)).compile.lastOrError
        decoded.get._1 mustBe num
      }
    }

    "parse array" in new Context {
      import jsonParse._
      forAll(jsArrGen(12, Gen.oneOf(jstrGen, jstrGen))) { js =>
        val js = JArray(List.fill(12)(JString("")))
        val i  = (js.arr.size / 2)

        val jsonStr = compact(render(js)).toCharArray

        val decoded =
          parseArrayItem(i, jsonParse.parseString)(path)(Stream(jsonStr: _*)).head.compile.lastOrError

        decoded.get._1 mustBe a[JString]
      }
    }
    "parse array specific" in new Context {
      import jsonParse._
      val jsonStr = compact(render(JArray(List(JString(""))))).toCharArray

      val decoded =
        parseArrayItem(0, parseString)(path)(Stream(jsonStr: _*)).head.compile.lastOrError

      decoded.get._1 mustBe a[JString]
    }
    "parse object" in new Context {
      forAll(jsObjGen("myKey", jsArrGen(2, jstrGen))) { obj =>
        val jsonStr = compact(render(obj)).toCharArray

        val ops     = Start.getKey("myKey").getN(1).getString.t
        val decoded = parseJSStream(ops, Stream(jsonStr: _*))
        decoded.get mustBe a[JString]
      }
    }

    "testbed" in new Context {
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
      val decoded1 = parseJSStream(ops.t, Stream(str: _*))

      decoded1.get mustBe a[JBool]
    }

    "work" in new Context {
      forAll(opsJsGen(5)) {
        case (ops, js) =>
          val jsStr = compact(render(js)).toCharArray

          val decoded = parseJSStream(Fix(ops), Stream(jsStr: _*))

          decoded.get mustBe a[JValue]
      }
    }
  }
}

trait Context {
  implicit val path: Vector[PPath]                         = Vector.empty
  implicit val jsonParse: JsonParse[Stream[IO, ?], JValue] = StreamJsonParser
  def parseJSStream(ops: Fix[ParseOps], s: Stream[IO, Char]): IO[JValue] =
    Parser.parseJS[Stream[IO, ?], JValue](ops, s)(jsonParse).head.compile.last.map {
      case Some((js, _)) => js
      case None          => throw ParseFailure.termination
    }
  implicit class Ops[A](private val io: IO[A]) {
    def get(): A = Await.result(io.unsafeToFuture(), 2.seconds)
  }
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
      n        <- Gen.choose(0, 15)
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
    s <- Gen.alphaStr
  } yield {
    JString(s)
  }

  val jnumGen = for {
    n <- Gen.choose(Double.MinValue, Double.MaxValue)
  } yield {
    JDouble(n / 2)
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
