package basil.api

import basil.parser._
import matryoshka.data.Fix
import matryoshka.implicits._
import matryoshka.Algebra
import scalaz._
import Arrow._
import org.json4s._

object Parser {

  import StringParse._
  val arrayParse = new ArrayParse(StringParse)
  import arrayParse._

  val parsing: Algebra[ParseOps, ParseAction[Parsed[JValue]]] = {
    case GetString     => parseString
    case GetN(n, next) => parseArray(n).andThen(next)
  }

  def parse(parseTree: OpsTree): ParseAction[Parsed[JValue]] = {
    parseTree.cata(parsing)
  }
}

sealed trait ParsingCtx
case object RootC extends ParsingCtx
case object ArrayRoot extends ParsingCtx
case object ObjectC extends ParsingCtx
case class ArrayMid(hasComma: Boolean) extends ParsingCtx

case class ParseError(msg: String)
