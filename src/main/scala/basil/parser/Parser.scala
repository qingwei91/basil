package basil.parser

import basil.data._
import ParseOps.ParseOpsHFunctor
import basil.typeclass.EffStack

import scala.util.{Failure, Success, Try}

object Parser {

  /**
    * The canonical entry point of this library, to extract certain
    * piece of data from a data source
    *
    * @param expr The parse operations tree that describe data
    *             user wanted
    * @param src Data source
    * @tparam JV User supplied Json AST type
    * @return
    */
  def parseSource[Source[_], I](expr: HFix[ParseOps, I], src: Source[Char])(
      implicit parse: JsonParse[Source]): Source[(I, parse.CharSource)] = {
    HFix.cata[ParseOps, I, parse.Parse](expr, parse.parsing).apply(initPath)(src)
  }

  // this method might be ineffcient, benchmark needed
  def parseString[I](expr: HFix[ParseOps, I], src: String)(
      implicit parse: JsonParse[EffStack[Try, List, ?]]): Try[I] = {
    HFix
      .cata[ParseOps, I, parse.Parse](expr, parse.parsing)
      .apply(initPath)(Success(src.toList))
      .flatMap {
        _.headOption
          .map(_._1)
          .fold[Try[I]](Failure(ParseFailure.termination(initPath)))(
            i => Success(i)
          )
      }
  }

  val initPath = Vector.empty[PPath]
}

object digit {
  private val digits = (0 to 9).map(x => Character.forDigit(x, 10))
  def unapply(arg: Char): Option[Char] = {
    Some(arg).filter(digits.contains)
  }
  def notDigit(char: Char): Boolean = !digits.contains(char)
}
object sign {
  def unapply(arg: Char): Option[Char] = {
    if (arg == '-' || arg == '+') {
      Some(arg)
    } else {
      None
    }
  }
}
object exponent {
  def unapply(c: Char): Option[Char] = if (c == 'e' || c == 'E') { Some(c) } else None
}
object whitespace {
  private val ws = List(' ', '\n', '\t', '\r')

  def unapply(arg: Char): Option[Char] = Some(arg).filter(ws.contains)
}
