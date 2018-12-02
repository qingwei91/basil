package basil.derive

import basil.data.ParseOpsConstructor._
import basil.data.{GetMultiple, ParseOps}
import basil.parser.Parser
import basil.parser.implicits._
import cats.free.FreeApplicative
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{pretty, render}
import org.scalatest.{MustMatchers, WordSpec}

import scala.util.Success

class DeriveParseSpec extends WordSpec with MustMatchers {
  case class Person(name: String, age: Double)
  case class Order(id: String, size: String, belongsTo: Person)

  def getMultiple[F[_]](implicit f: FreeApplicative[ParseOps[F, ?], Order]) = GetMultiple(f)

  "Able to derive ParseOp for case class" in {
    import DeriveParseOps._

    val what     = Start.getI[Order].t
    val js       = ("id" -> "hoho") ~ ("size" -> "20") ~ ("belongsTo" -> ("name" -> "Qing") ~ ("age" -> 20))
    val jsString = pretty(render(js))
    val res      = Parser.parseString(what, jsString)

    res mustBe Success(Order("hoho", "20", Person("Qing", 20)))
  }
}
