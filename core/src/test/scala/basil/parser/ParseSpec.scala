package basil
package parser

import basil.data._
import basil.parser.PContext._
import basil.syntax.ParseOpsConstructor._
import cats.arrow.FunctionK
import cats.free.FreeApplicative
import cats.implicits._
import cats.{Applicative, Show, ~>}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods.{compact, pretty, render}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.specs2._

abstract class ParseSpec[Input, Output[_]] extends Specification with ScalaCheck with ParserGen {
  override def is =
    s2"""Parser should
        - parse String          - $parseStringSpec
        - parse Bool            - $parseBoolSpec
        - parse Number          - $parseNumSpec
        - parse Object          - $parseObjSpec
        - parse Array           - $parseArrSpec
        - parse Any JS          - $parseRandomJSSpec
        - parse Partial Array   - $parsePartialArrSpec
        - parse Partial Object  - $parsePartialObjSpec
        - parse Product type    - $parseProductTypeSpec
        - parse Optional value  - $parseOptionalSpec
        - NOT parse invalid num - $parseInvalidNumSpec
      """

  // JsonParse implementation to test
  implicit val parser: JsonParse[Input, Output]

  implicit def jsPretty2(input: (ParseTree[_], JValue, Any)): Pretty = {
    Pretty { params =>
      val (tree, js, any) = input
      val treeS           = tree.show
      val jsStr           = pretty(render(js))
      val expected        = any
      s"""
         |Tree: $treeS
         |JS  : $jsStr
         |Exp : $expected
       """.stripMargin
    }
  }
  implicit def jsPretty(jsPair: (JValue, JString)): Pretty = {
    Pretty { params =>
      val s1 = pretty(render(jsPair._1))
      val s2 = jsPair._2.values
      s"""$s1
         |and
         |  $s2
       """.stripMargin
    }
  }

  // A way to lift char array to the Source effect for testing
  def liftF(charArr: Array[Char]): Input

  // get the last element from F[A], with the potential of
  // throwing
  def getLast[A](f: Output[A]): A

  implicit class FOps[I](f: Output[I]) {
    def getVal: I = getLast(f)
  }
  implicit class GenOps[A](gen: Gen[A]) {
    def branch: Gen[(A, A)] = gen.map(s => s -> s)
  }

  def parseStringSpec = {
    import parser._
    forAll(jstrGen) { js =>
      val jsonStr = compact(render(js)).toCharArray
      val decoded = parseString(path)(liftF(jsonStr)).getVal

      decoded must_=== js.values
    }
  }

  def parseBoolSpec = {
    import parser._
    List(JBool.True, JBool.False)
      .map { js =>
        val jsonStr = compact(render(js)).toCharArray
        val decoded = parseBoolean(path)(liftF(jsonStr)).getVal

        decoded must_=== js.value
      }
      .reduce(_ and _)
  }

  def parseNumSpec = {
    import parser._
    forAll(jnumGen) { num =>
      val jsonStr = compact(render(num)).toCharArray
      val decoded = parseNumber(End)(path)(liftF(jsonStr)).getVal
      decoded must_=== num.values
    }
  }

  def parseInvalidNumSpec = {
    import parser._

    val jsonStr = "e21".toCharArray

    parseNumber(End)(path)(liftF(jsonStr)).getVal must throwA[NumberFormatException]
  }

  def parseArrSpec = {
    import parser._
    val i = 12
    forAll[(JValue, JString), Boolean](jsArrGen(i, jstrGen.branch)) {
      case (js, expected) =>
        val jsonStr = compact(render(js)).toCharArray

        val decoded =
          parseArrayItem(i, parser.parseString)(path)(liftF(jsonStr)).getVal

        decoded must_=== expected.values
    }
  }

  def parseObjSpec = {
    forAll[(JValue, JString), Boolean](jsObjGen("myKey", jsArrGen(2, jstrGen.branch))) {
      case (obj, expected) =>
        val jsonStr = compact(render(obj)).toCharArray

        val ops     = Start.getKey("myKey").getN(2).getString.eval
        val decoded = parseJSStream(ops, liftF(jsonStr)).getVal
        decoded must_=== expected.values
    }
  }

  def parseRandomJSSpec = {
    forAll(opsJsGen(5)) {
      case (ops, inputJS, expected) =>
        val jsStr = compact(render(inputJS)).toCharArray

        val decoded = parseJSStream(ops, liftF(jsStr)).getVal

        decoded must_=== expected
    }
  }

  def parsePartialArrSpec = {

    val arr: JArray  = List(2, 3, 10, 20, 31)
    val partialJsStr = compact(render(arr)).toCharArray.dropRight(6)
    val ops          = Start.getN(2).getNum.eval

    val decoded = parseJSStream(ops, liftF(partialJsStr)).getVal

    decoded must_=== 10.0
  }

  def parsePartialObjSpec = {
    val obj = ("keyA" -> "aa") ~
      (""          -> ("oh my" -> 20) ~ ("really?" -> "nope")) ~
      ("{wontwork" -> List(true, false))

    val partialJsStr = compact(render(obj)).toCharArray.dropRight(15)
    val ops          = Start.getKey("").getKey("really?").getString.eval

    val decoded = parseJSStream(ops, liftF(partialJsStr)).getVal
    decoded must_=== "nope"
  }

  def parseProductTypeSpec = {
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

    val string = compact(render(obj)).toCharArray

    val decoded = parseJSStream(ops.eval, liftF(string)).getVal
    decoded must_=== (("Qing", 201, 20))
  }

  def parseOptionalSpec = {
    val obj: JValue = ("outer" -> (
      "inner" -> "string"
    ))
    val jsStr = compact(render(obj)).toCharArray
    val ops   = Start.getKey("outer").getOpt.getKey("missing").getString.eval

    val decoded = parseJSStream(ops, liftF(jsStr)).getVal
    decoded must_=== None
  }

}

private object PContext {
  implicit val path: Vector[PPath] = Vector.empty

  def parseJSStream[Input, Output[_], I](ops: HFix[ParseOps, I], s: Input)(
      implicit parser: JsonParse[Input, Output]): Output[I] =
    Parser.parseG[Input, Output, I](ops, s)(parser)

  type JustString[A] = String

  implicit val justStringApp: Applicative[JustString] = new Applicative[JustString] {
    override def pure[A](x: A): JustString[A] = x.toString

    override def ap[A, B](ff: JustString[A => B])(fa: JustString[A]): JustString[B] = ff.concat(fa)
  }
  val showAlg: ParseOps[JustString, ?] ~> JustString =
    new (ParseOps[JustString, ?] ~> JustString) {
      override def apply[A](fa: ParseOps[JustString, A]): JustString[A] = {
        fa match {
          case GetString => ".getString"
          case GetBool   => ".getBool"
          case GetNum(t) => s".getNum.<$t>"
          case getN: GetN[JustString, i] =>
            val n = getN.n
            s".getN($n)${getN.next}"
          case getK: GetKey[JustString, i] =>
            val key = getK.key
            s".getKey($key)${getK.next}"
          case GetSum(oneOf) =>
            val oneOfStr = oneOf.toNel.foldLeft("") {
              case (str, (tpe, next)) =>
                s"$str\n\t$tpe: ${next.value}"
            }
            s".oneOf($oneOfStr)"
          case getOpt: GetOpt[JustString, i] => s".?${getOpt.next}"
          case _: Mapped[JustString, h, A]   => ""
          case GetProduct(all)               => all.foldMap(FunctionK.id)
        }
      }
    }
  implicit def ParseTreeShow[I]: Show[ParseTree[I]] = {
    new Show[ParseTree[I]] {
      override def show(t: ParseTree[I]): String = {
        HFix.cata[ParseOps, I, JustString](t, showAlg)
      }
    }
  }
}
