package basil.parser

import basil.data._
import schemes.{Fix, Schemes}

object Parser {

  def parseJS[Source[_], JV](expr: Fix[ParseOps], src: Source[Char])(
      implicit parse: JsonParse[Source, JV]): Source[(JV, parse.CharSource)] = {
    Schemes.cata(expr)(parse.parsing).apply(Vector.empty)(src)
  }
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
