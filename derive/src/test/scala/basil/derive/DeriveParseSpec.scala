package basil.derive

import basil.ParseTree
import basil.data.ParseOps.NestedParseOpsFunctor
import basil.derive.DeriveParseOps._
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import cats.syntax.functor._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{pretty, render}
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.Success

class DeriveParseSpec extends WordSpec with MustMatchers {
  sealed trait Status
  case object Married extends Status
  case object Single  extends Status

  case class Person(name: String, age: Double, married: Status, life: Option[String])
  case class Order(id: String, size: String, belongsTo: Person)

  implicit val StringIsMarried: ParseTree[Single.type] = getString
    .map(_ => Single)

  implicit val BoolIsMarried: ParseTree[Married.type] = getBool
    .map {
      case true  => Married
      case false => Married
    }

  "Able to derive ParseOp for nested case class" in {
    val what = Start.getType[Order].eval
    val js = ("id" -> "hoho") ~
      ("size" -> "20") ~
      ("belongsTo" ->
        ("name"      -> "Qing") ~
          ("age"     -> 20) ~
          ("married" -> true) ~
          ("life"    -> "hoho"))

    val jsString = pretty(render(js))
    val res      = Parser.parseString(what, jsString)

    res mustBe Success(Order("hoho", "20", Person("Qing", 20, Married, Some("hoho"))))
  }

  sealed trait Dir
  case class Left(i: String)      extends Dir
  case class Right(i: String)     extends Dir
  case class More(a: Dir, b: Dir) extends Dir

  "Able to derive recursive ADT" in {
    val x = Start.getType[Dir].eval

    Parser.parseString(x, "sss")
    println(x)
  }
}
