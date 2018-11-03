package basil.data

sealed trait ExpectedTerminator
case object Comma                             extends ExpectedTerminator
case object Bracket                           extends ExpectedTerminator
case object CurlyBrace                        extends ExpectedTerminator
case object End                               extends ExpectedTerminator
case class OneOf(t: List[ExpectedTerminator]) extends ExpectedTerminator

object ExpectedTerminator {
  implicit class Ops(terminator: ExpectedTerminator) {
    def matchChar(char: Char): Boolean = {
      terminator match {
        case Comma      => char == ','
        case Bracket    => char == ']'
        case CurlyBrace => char == '}'
        case OneOf(t)   => t.exists(_.matchChar(char))
        case End        => false
      }
    }
  }
}

object OneOf {
  def apply(t: ExpectedTerminator*): OneOf = OneOf(List(t: _*))
}
