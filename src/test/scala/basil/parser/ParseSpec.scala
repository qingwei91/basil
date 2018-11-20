package basil.parser

import basil.data.ParseOpsConstructor._
import basil.data._
import cats.Functor
import cats.syntax.functor._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods.{pretty, render}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, WordSpec}

abstract class ParseSpec[F[_]: Functor]
    extends WordSpec
    with MustMatchers
    with GeneratorDrivenPropertyChecks
    with ParserGen {

  // JsonParse implementation to test
  implicit val parser: JsonParse[F]

  // A way to lift char array to the Source effect for testing
  def liftF(charArr: Array[Char]): F[Char]

  // get the last element from F[A], with the potential of
  // throwing
  def getLast[A](f: F[A]): A

  private implicit class FOps[I, A](f: F[(I, A)]) {
    def getVal: I = getLast(f.map(_._1))
  }

  "parser" should {
    "parse string" in new PContext[F] {
      import parser._
      forAll(jstrGen) { js =>
        val jsonStr = pretty(render(js)).toCharArray
        val decoded = parseString(path)(liftF(jsonStr)).getVal

        decoded mustBe js.values
      }
    }
    "parse boolean" in new PContext[F] {
      import parser._
      List(JBool.True, JBool.False).foreach { js =>
        val jsonStr = pretty(render(js)).toCharArray
        val decoded = parseBoolean(path)(liftF(jsonStr)).getVal

        decoded mustBe js.value
      }
    }
    "parse number" in new PContext[F] {
      import parser._
      forAll(jnumGen) { num =>
        val jsonStr = pretty(render(num)).toCharArray
        val decoded = parseNumber(End)(path)(liftF(jsonStr)).getVal
        decoded mustBe num.values
      }
    }
    "parse array" in new PContext[F] {
      import parser._
      val i = 12
      forAll(jsArrGen(i, jstrGen.branch)) {
        case (js, expected) =>
          val jsonStr = pretty(render(js)).toCharArray

          val decoded =
            parseArrayItem(i, parser.parseString)(path)(liftF(jsonStr)).getVal

          decoded mustBe expected.values
      }
    }
    "parse object" in new PContext[F] {
      forAll(jsObjGen("myKey", jsArrGen(2, jstrGen.branch))) {
        case (obj, expected) =>
          val jsonStr = pretty(render(obj)).toCharArray

          val ops     = Start[String].getKey("myKey").getN(2).getString.t
          val decoded = parseJSStream(ops, liftF(jsonStr)).getVal
          decoded mustBe expected.values
      }
    }
    "parse random JS" in new PContext[F] {
      forAll(opsJsGen(5)) {
        case (ops, js, extracted) =>
          val jsStr = pretty(render(js)).toCharArray

          val decoded = parseJSStream(HFix(ops), liftF(jsStr)).getVal

          decoded mustBe extracted.values
      }
    }
    "parse partial array" in new PContext[F] {
      val arr: JArray  = List(2, 3, 10, 20, 31)
      val partialJsStr = pretty(render(arr)).toCharArray.dropRight(6)
      val ops          = Start[Double].getN(2).getNum.t

      val decoded = parseJSStream(ops, liftF(partialJsStr)).getVal

      decoded mustBe 10.0
    }

    "parse partial obj" in new PContext[F] {
      val obj = (
        ("keyA"        -> "aa") ~
          (""          -> ("oh my" -> 20) ~ ("really?" -> "nope")) ~
          ("{wontwork" -> List(true, false))
      )

      val partialJsStr = pretty(render(obj)).toCharArray.dropRight(15)
      val ops          = Start[String].getKey("").getKey("really?").getString.t

      val decoded = parseJSStream(ops, liftF(partialJsStr)).getVal
      decoded mustBe "nope"
    }
  }
}

private abstract class PContext[F[_]](implicit val parser: JsonParse[F]) {
  implicit val path: Vector[PPath] = Vector.empty

  def parseJSStream[I](ops: HFix[ParseOps, I], s: F[Char]): F[(I, F[Char])] =
    Parser.parseJS[F, I](ops, s)(parser)
}

trait ParserGen {
  type OpTree[I] = ParseOps[HFix[ParseOps, ?], I]

  private def endingOpGen: Gen[ExpectedTerminator => ParseOps[Nothing, _]] = {
    val s: ExpectedTerminator => ParseOps[Nothing, String]  = _ => GetString
    val n: ExpectedTerminator => ParseOps[Nothing, Double]  = x => GetNum(x)
    val b: ExpectedTerminator => ParseOps[Nothing, Boolean] = _ => GetBool

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
      op <- Gen.oneOf[OpTree[_]](GetN(n, HFix(endingOp(OneOf(Bracket, Comma)))),
                                 GetKey(key, HFix(endingOp(OneOf(Comma, CurlyBrace)))))
    } yield { _: ExpectedTerminator =>
      op
    }
  }

  def opsGen(depth: Int): Gen[OpTree[_]] = {
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

  def opsJsGen(depth: Int): Gen[(OpTree[_], JValue, JValue)] = {
    for {
      ops                 <- opsGen(depth)
      (jValue, extracted) <- jsGen(ops)
    } yield (ops, jValue, extracted)
  }

  /**
    *
    * @param op
    * @return Generate a pair of JsValue,
    *         _._1 is the nested JsValue
    *         _._2 is the drilldown value that should be extracted by our parsing
    */
  def jsGen[I](op: OpTree[I]): Gen[(JValue, JValue)] = {
    op match {
      case GetString         => jstrGen.branch
      case GetNum(_)         => jnumGen.branch
      case GetBool           => Gen.oneOf(JBool.False, JBool.True).branch
      case GetN(n, next)     => jsArrGen(n, jsGen(next.unfix))
      case GetKey(key, next) => jsObjGen(key, jsGen(next.unfix))
    }
  }

  private val specialChar = Gen.oneOf("\\b", "\\r", "\\f", "\\\\", "\\/")

  val jstrGen =
    for {
      raw <- Gen.asciiPrintableStr.suchThat(_.length < 200)
      sc  <- specialChar
    } yield {
      // todo: have special test case for escape char
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

  def jsObjGen(key: String, gen: Gen[(JValue, JValue)]): Gen[(JObject, JValue)] = {
    for {
      n                   <- Gen.choose(2, 8)
      obj                 <- randomObj(n)
      (nested, drillDown) <- gen
    } yield {
      val merged = (key -> nested) :: obj.obj

      JObject(merged) -> drillDown
    }
  }

  def randomPair = {
    for {
      key   <- Gen.alphaNumStr.suchThat(s => s.length < 200)
      value <- randomJsGen
    } yield {
      key -> value
    }
  }

  def randomObj(n: Int) = {
    Gen.listOfN(n, randomPair).map { kvs =>
      JObject(kvs)
    }
  }

  def randomJsGen: Gen[JValue] = {

    Gen.frequency(
      6 -> jstrGen,
      6 -> jnumGen,
      6 -> jBoolGen,
      1 -> Gen.listOfN(5, Gen.lzy(randomJsGen)).map(ls => JArray(ls)),
      1 -> Gen.choose[Int](2, 12).flatMap(randomObj)
    )
  }

  def jsArrGen(n: Int, gen: Gen[(JValue, JValue)]) = {
    for {
      list               <- Gen.listOfN(n + 1, randomJsGen)
      (target, finalVal) <- gen
    } yield {
      val (a, b) = list.splitAt(n)
      JArray(a ::: (target :: b)) -> finalVal
    }
  }
}
