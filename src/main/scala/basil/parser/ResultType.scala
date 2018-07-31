package basil.parser

import scalaz.Apply
import simulacrum._

@typeclass
trait ResultType[+A] {
  type R
}

object ResultType {
//  implicit val noResult: ParseResult[Nothing] = new ParseResult[Nothing] {
//    type R = Nothing
//  }
}
