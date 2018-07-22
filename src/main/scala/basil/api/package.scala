package basil

import basil.parser.ParseOps
import matryoshka.data.Fix
import scalaz.Kleisli

package object api {
  type OpsTree = Fix[ParseOps]

  case class ParsingState(raw: String, idx: Int, ctx: ParsingCtx) {
    def idxIncrement(x: Int): ParsingState = copy(idx = idx + x)
    def withCtx(parsingCtx: ParsingCtx): ParsingState = copy(ctx = parsingCtx)
  }

  case class Parsed[A](endedAt: Int, value: A)

  type ParseResult[A] = Either[ParseError, A]
  type ParseAction[A] = Kleisli[ParseResult, ParsingState, A]
}
