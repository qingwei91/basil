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
  import NumParse._
  val arrayParse = new ArrayParse(StringParse)
  import arrayParse._

  val parsing: Algebra[ParseOps, ParseAction[Parsed[JValue]]] = {
    case GetString     => parseString
    case GetN(n, next) => parseArray(n).andThen(next)
    case GetNum => parseNum
  }

  def parse(parseTree: OpsTree): ParseAction[Parsed[JValue]] = {
    parseTree.cata(parsing)
  }
}

sealed trait ParsingCtx
case object RootC extends ParsingCtx
case object ArrayC extends ParsingCtx
case object ObjC extends ParsingCtx

case class ParseError(msg: String)
