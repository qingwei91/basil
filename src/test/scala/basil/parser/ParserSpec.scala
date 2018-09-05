package basil.parser

import fs2.Stream
import org.scalatest._
import ParseOpsConstructor._
import org.json4s._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen

import org.json4s.native.JsonMethods._

class ParserSpec extends WordSpec with MustMatchers with GeneratorDrivenPropertyChecks {
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

  val jsonGen = for {
    x <- Gen.oneOf(jstrGen, jnumGen)
  } yield {
    x
  }

  def jsArrGen(gen: Gen[JValue]) = {
    for {
      n  <- Gen.choose(1, 15)
      js <- Gen.listOfN(n, gen)
    } yield {
      JArray(js)
    }
  }

  "new parser" should {
    "parse string" in {
      forAll(jstrGen) { js =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded = parseString(Stream.apply(jsonStr: _*))

        decoded.unsafeRunSync() mustBe js
      }
    }
    "parse array" in {
      forAll(jsArrGen(jstrGen)) { js =>
        val i       = (js.arr.size / 2)

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
  }
}
