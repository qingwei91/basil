package basil.parser

sealed trait ExpectedTerminator
case object Comma      extends ExpectedTerminator
case object Bracket    extends ExpectedTerminator
case object CurlyBrace extends ExpectedTerminator
case object End        extends ExpectedTerminator

object ExpectedTerminator {
  implicit class Ops(terminator: ExpectedTerminator) {
    def matchChar(char: Char): Boolean = {
      terminator match {
        case Comma      => char == ','
        case Bracket    => char == ']'
        case CurlyBrace => char == '}'
        case End        => false
      }
    }
  }
}
