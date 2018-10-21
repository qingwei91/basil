package basil.parser

import basil.End
import fs2.Stream
import matryoshka.data.Fix
import org.json4s._
import org.json4s.native.JsonMethods._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserSpec
    extends WordSpec
    with MustMatchers
    with GeneratorDrivenPropertyChecks
    with ParserGen {

  "new parser" should {
    "parse string" in {
      forAll(jstrGen) { js =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded = parseString(Stream.apply(jsonStr: _*))

        decoded.unsafeRunSync() mustBe js
      }
    }
    "parse boolean" in {
      List(JBool.True, JBool.False).foreach { js =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded = parseBoolean(Stream.apply(jsonStr: _*))

        decoded.unsafeRunSync() mustBe js
      }
    }
    "parse number" in {
      forAll(jnumGen) { num =>
        val jsonStr = compact(render(num)).toCharArray
        val decoded = parseNumber(End)(Stream.apply(jsonStr: _*))
        decoded.unsafeRunSync() mustBe num
      }
    }
    "parse array" in {
      forAll(jsArrGen(12, Gen.oneOf(jstrGen, jstrGen))) { js =>
        val i = (js.arr.size / 2)

        val jsonStr = compact(render(js)).toCharArray

        val decoded =
          parseArrayItem(i, parseString)(Stream(jsonStr: _*))

        decoded.unsafeRunSync() mustBe a[JString]
      }
    }
    "parse array specific" in {
      val jsonStr = compact(render(JArray(List(JString(""))))).toCharArray

      val decoded =
        parseArrayItem(0, parseString)(Stream(jsonStr: _*))
      decoded.unsafeRunSync() mustBe a[JString]
    }
    "parse object" in {
      forAll(jsObjGen("myKey", jsArrGen(2, jstrGen))) { obj =>
        val jsonStr = compact(render(obj)).toCharArray

        val decoded = parseObj("myKey", parseArrayItem(1, parseString))(Stream(jsonStr: _*))
        decoded.unsafeRunSync() mustBe a[JString]
      }
    }
    "work" ignore {
      forAll(opsGen(5)) { ops =>
        val js    = jsGen(ops).sample.get
        val jsStr = compact(render(js)).toCharArray

        val decoded = parse(Fix(ops))(Stream(jsStr: _*))

        decoded.unsafeRunSync() mustBe js
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
      key <- Gen.asciiStr
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

  def jsGen(op: ParseOps[Fix[ParseOps]]): Gen[JValue] = {
    op match {
      case GetString         => jstrGen
      case GetNum            => jnumGen
      case GetBool           => Gen.oneOf(JBool.False, JBool.True)
      case GetNullable(ops)  => Gen.some(jsGen(ops.unFix)).map(_.fold[JValue](JNull)(identity))
      case GetN(n, next)     => jsArrGen(n, jsGen(next.unFix))
      case GetKey(key, next) => jsObjGen(key, jsGen(next.unFix))
    }
  }

  val jstrGen = for {
    s <- Gen.alphaNumStr
  } yield {
    JString(s)
  }

  val jnumGen = for {
    n <- Gen.choose(-10000, 10000)
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
