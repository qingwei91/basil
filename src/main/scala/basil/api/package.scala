package basil

import basil.parser.{ParseOps, ResultType}
import matryoshka.data.Fix
import matryoshka.{EqualT, RecursiveT, ShowT}
import scalaz.{Functor, Kleisli}

package object api {
  type OpsTree = Fix[ParseOps]

  // todo: benchmark and see if mutate is more performant
  case class ParsingState(raw: Array[Char], idx: Int, ctx: ParsingCtx) {
    def idxIncrement(x: Int): ParsingState = copy(idx = idx + x)
    def withCtx(parsingCtx: ParsingCtx): ParsingState = copy(ctx = parsingCtx)

    def charAt(i: Int): Char = raw(i)
    def pointedChar: Char = raw(idx)
    def isLast: Boolean = raw.length == idx + 1
  }

  case class Parsed[A](endedAt: Int, value: A)

  type ParseResult[A] = Either[ParseError, A]
  type ParseAction[A] = Kleisli[ParseResult, ParsingState, A]

  private val whiteSpaceChars = List(' ', '\n')
  object whitespaces {
    def unapply(arg: Char): Option[Char] = {
      whiteSpaceChars.find(_ == arg)
    }
  }
}

case class MyFix[F[_]](unFix: F[MyFix[F]])(
    implicit val rTpe: ResultType[F[MyFix[F]]]) {
  type R = rTpe.R
}

object MyFix {
  implicit def birecursiveT: RecursiveT[MyFix] = new RecursiveT[MyFix] {
    def projectT[F[_]: Functor](t: MyFix[F]): F[MyFix[F]] = t.unFix
  }

  implicit val equalT: EqualT[Fix] = EqualT.recursiveT

  implicit val showT: ShowT[Fix] = ShowT.recursiveT
}
