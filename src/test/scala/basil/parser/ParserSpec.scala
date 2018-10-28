package basil.parser

import fs2.Stream
import matryoshka.data.Fix
import org.json4s._
import org.json4s.native.JsonMethods._
import org.scalacheck._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import basil.parser.Parser._

class ParserSpec
    extends WordSpec
    with MustMatchers
    with GeneratorDrivenPropertyChecks
    with ParseOpsConstructor
    with ParserGen {
  implicit val path: Vector[PPath] = Vector.empty

  "new parser" should {
    "parse string" in {
      forAll(jstrGen) { js =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded = parseString(path)(Stream.apply(jsonStr: _*))

        decoded.unsafeRunSync()._1 mustBe js
      }
    }
    "parse boolean" in {
      List(JBool.True, JBool.False).foreach { js =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded = parseBoolean(path)(Stream.apply(jsonStr: _*))

        decoded.unsafeRunSync()._1 mustBe js
      }
    }
    "parse number" in {
      forAll(jnumGen) { num =>
        val jsonStr = compact(render(num)).toCharArray
        val decoded = parseNumber(End)(path)(Stream.apply(jsonStr: _*))
        decoded.unsafeRunSync()._1 mustBe num
      }
    }
    "parse array" in {
      forAll(jsArrGen(12, Gen.oneOf(jstrGen, jstrGen))) { js =>
        val i = (js.arr.size / 2)

        val jsonStr = compact(render(js)).toCharArray

        val decoded =
          parseArrayItem(i, parseString)(path)(Stream(jsonStr: _*))

        decoded.unsafeRunSync()._1 mustBe a[JString]
      }
    }
    "parse array specific" in {
      val jsonStr = compact(render(JArray(List(JString(""))))).toCharArray

      val decoded =
        parseArrayItem(0, parseString)(path)(Stream(jsonStr: _*))
      decoded.unsafeRunSync()._1 mustBe a[JString]
    }
    "parse object" in {
      forAll(jsObjGen("myKey", jsArrGen(2, jstrGen))) { obj =>
        val jsonStr = compact(render(obj)).toCharArray

        val ops     = Start.getKey("myKey").getN(1).getString.t
        val decoded = Parser.parse(ops, Stream(jsonStr: _*))
        decoded.unsafeRunSync() mustBe a[JString]
      }
    }

    "testbed" in {

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

      val str     = compact(render(obj)).toCharArray
      val decoded = Parser.parse(ops.t, Stream(str: _*))
      decoded.unsafeRunSync() mustBe a[JBool]
    }

    "work" in {
      forAll(opsJsGen(5)) {
        case (ops, js) =>
          val jsStr = compact(render(js)).toCharArray

          val decoded = Parser.parse(Fix(ops), Stream(jsStr: _*))

          decoded.unsafeRunSync() mustBe a[JValue]
      }
    }
  }
}

trait ParserGen {
  def endingOpGen: Gen[ParseOps[Nothing]] = Gen.oneOf(GetString, GetNum, GetBool)

  def nonEndingOpGen(gen: Gen[ParseOps[Fix[ParseOps]]]): Gen[ParseOps[Fix[ParseOps]]] = {
    for {
      a   <- gen
      n   <- Gen.choose(0, 15)
      key <- Gen.alphaStr.filter(_.nonEmpty)
      op  <- Gen.oneOf(GetNullable(Fix(a)), GetN(n, Fix(a)), GetKey(key, Fix(a)))
    } yield {
      op
    }
  }

  def opsGen(depth: Int): Gen[ParseOps[Fix[ParseOps]]] = {
    depth match {
      case 0 => endingOpGen
      case n => nonEndingOpGen(opsGen(n - 1))
    }
  }

  def opsJsGen(depth: Int): Gen[(ParseOps.ParseOpsF, JValue)] = {
    for {
      ops    <- opsGen(depth)
      jValue <- jsGen(ops)
    } yield ops -> jValue
  }

  def jsGen(op: ParseOps[Fix[ParseOps]]): Gen[JValue] = {
    op match {
      case GetString         => jstrGen
      case GetNum            => jnumGen
      case GetBool           => Gen.oneOf(JBool.False, JBool.True)
      case GetNullable(ops)  => Gen.some(jsGen(ops.unFix)).map(_.fold[JValue](JNull)(identity))
      case GetN(n, next)     => jsArrGen(n + 1, jsGen(next.unFix))
      case GetKey(key, next) => jsObjGen(key, jsGen(next.unFix))
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
