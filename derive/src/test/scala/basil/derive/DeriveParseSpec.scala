package basil.derive

import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{pretty, render}
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.Success

class DeriveParseSpec extends WordSpec with MustMatchers {
  sealed trait Status
  case object Married extends Status
  case object Single  extends Status

  case class Person(name: String, age: Double, married: Boolean, life: Option[String])
  case class Order(id: String, size: String, belongsTo: Person)

  "Able to derive ParseOp for nested case class" in {
    import DeriveParseOps._

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

    res mustBe Success(Order("hoho", "20", Person("Qing", 20, true, Some("hoho"))))
  }
}
