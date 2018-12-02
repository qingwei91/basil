package basil.magnolia

import basil.data.ParseOpsConstructor._
import basil.data.{GetMultiple, ParseOps}
import basil.parser.implicits._
import basil.parser.Parser
import cats.free.FreeApplicative
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{pretty, render}
import org.scalatest.{MustMatchers, WordSpec}

class DeriveParseSpec extends WordSpec with MustMatchers {
  case class Order(id: String, size: String)

  def getMultiple[F[_]](implicit f: FreeApplicative[ParseOps[F, ?], Order]) = GetMultiple(f)

  "Able to derive ParseOp for case class" in {
    import DeriveParseOps._

    val what     = Start.getI[Order].t
    val js       = ("id" -> "hoho") ~ ("size" -> "20")
    val jsString = pretty(render(js))
    val res      = Parser.parseString(what, jsString)
    println(res)
  }
}
