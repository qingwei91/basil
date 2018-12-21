package basil.parser

import basil.data._
import basil.syntax.ParseOpsConstructor._
import cats.Functor
import cats.free.FreeApplicative
import cats.implicits._
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

  implicit class FOps[I, A](f: F[(I, A)]) {
    def getVal: I = getLast(f.map(_._1))
  }
  implicit class GenOps[A](gen: Gen[A]) {
    def branch: Gen[(A, A)] = gen.map(s => s -> s)
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

          val ops     = Start.getKey("myKey").getN(2).getString.eval
          val decoded = parseJSStream(ops, liftF(jsonStr)).getVal
          decoded mustBe expected.values
      }
    }
    "parse random JS" in new PContext[F] {
      forAll(opsJsGen(5)) {
        case (ops, inputJS, expected) =>
          val jsStr = pretty(render(inputJS)).toCharArray

          val decoded = parseJSStream(ops, liftF(jsStr)).getVal

          decoded mustBe expected
      }
    }
    "parse partial array" in new PContext[F] {
      val arr: JArray  = List(2, 3, 10, 20, 31)
      val partialJsStr = pretty(render(arr)).toCharArray.dropRight(6)
      val ops          = Start.getN(2).getNum.eval

      val decoded = parseJSStream(ops, liftF(partialJsStr)).getVal

      decoded mustBe 10.0
    }
    "parse partial obj" in new PContext[F] {
      val obj = ("keyA" -> "aa") ~
        (""          -> ("oh my" -> 20) ~ ("really?" -> "nope")) ~
        ("{wontwork" -> List(true, false))

      val partialJsStr = pretty(render(obj)).toCharArray.dropRight(15)
      val ops          = Start.getKey("").getKey("really?").getString.eval

      val decoded = parseJSStream(ops, liftF(partialJsStr)).getVal
      decoded mustBe "nope"
    }
    "parse product type" in new PContext[F] {
      val obj = ("name" -> "Qing") ~
        ("age"  -> 201) ~
        ("love" -> true) ~ ("nest" -> ("down" -> 20))

      def wrap[A](o: HFix[ParseOps, A]): FreeApplicative[HFix[ParseOps, ?], A] = {
        FreeApplicative.lift(o)
      }

      val ops = Start.getAll(
        (
          wrap(Start.getKey("name").getString.eval),
          wrap(Start.getKey("age").getNum.eval),
          wrap(Start.getKey("nest").getKey("down").getNum.eval)
        ).mapN {
          case (a, b, c) => (a, b, c)
        }
      )

      val string = pretty(render(obj)).toCharArray

      val decoded = parseJSStream(ops.eval, liftF(string)).getVal
      decoded mustBe (("Qing", 201, 20))
    }
    "parse nested optional value" in new PContext[F] {
      val obj: JValue = ("outer" -> (
        "inner" -> "string"
      ))
      val jsStr = pretty(render(obj)).toCharArray
      val ops   = Start.getKey("outer").getOpt.getKey("missing").getString.eval

      val decoded = parseJSStream(ops, liftF(jsStr)).getVal
      decoded mustBe None
    }
  }
}

private abstract class PContext[F[_]](implicit val parser: JsonParse[F]) {
  implicit val path: Vector[PPath] = Vector.empty

  def parseJSStream[I](ops: HFix[ParseOps, I], s: F[Char]): F[(I, F[Char])] =
    Parser.parseSource[F, I](ops, s)(parser)
}
