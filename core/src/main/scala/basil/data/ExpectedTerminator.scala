package basil.data

/**
  * ADT to represent expected terminator when parsing json
  * it's only use when decoding number because number does not
  * comes with native terminator
  *
  * eg. it's evident that `"halo"` is a complete string,
  *     and `"halo` is not, but we cannot do the same for number
  *     `2000` and `20000` both can be incomplete
  *     depending on the subsequent characters
  */
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
