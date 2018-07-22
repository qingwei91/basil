package basil.api

import org.json4s.JValue
import scalaz.{Bind, Kleisli}
import scalaz.syntax.arrow._

class ArrayParse(stringParser: StringParse) {
  import stringParser._

  implicit val eitherBind: Bind[ParseResult] = new Bind[ParseResult] {
    override def bind[A, B](fa: ParseResult[A])(
        f: A => ParseResult[B]): ParseResult[B] = {
      fa.flatMap(f)
    }

    override def map[A, B](fa: ParseResult[A])(f: A => B): ParseResult[B] =
      fa.map(f)
  }

  implicit class ParseActionHelper[A](a1: ParseAction[A]) {
    def orElse(a2: ParseAction[A]): ParseAction[A] = Kleisli { st =>
      a1(st).fold(
        err => a2(st),
        jsV => Right(jsV)
      )
    }
  }

  private def parseAnElement: ParseAction[Parsed[JValue]] = parseString

  private val whiteSpaceChars = List(' ', '\n')
  object whitespaces {
    def unapply(arg: Char): Option[Char] = {
      whiteSpaceChars.find(_ == arg)
    }
  }

  private def parseUntilFirst(
      state: ParsingState,
      openBracketSeen: Boolean = false): Either[ParseError, ParsingState] = {
    val ParsingState(raw, idx, _) = state

    val charI = raw.charAt(idx)

    charI match {
      case whitespaces(ws) =>
        parseUntilFirst(state.idxIncrement(1), openBracketSeen)
      case '[' if !openBracketSeen =>
        parseUntilFirst(state.idxIncrement(1), openBracketSeen = true)
      case '[' =>
        Left(ParseError("[ seen more than once"))
      case _ if openBracketSeen => Right(state)
      case other =>
        Left(ParseError(s"Syntax err, expect open bracket, got $other"))
    }
  }

  private def parseUntilNext(
      state: ParsingState): Either[ParseError, ParsingState] = {
    val ParsingState(raw, idx, ctx) = state

    val charI = raw.charAt(idx)
    (charI, ctx) match {
      case (whitespaces(_), ArrayMid(_)) =>
        parseUntilNext(state.idxIncrement(1))
      case (',', ArrayMid(true)) =>
        Left(ParseError(s"Syntax error, multiple comma: $state"))
      case (',', ArrayMid(false)) =>
        parseUntilNext(state.idxIncrement(1).withCtx(ArrayMid(true)))
      case (_, ArrayMid(true)) => Right(state)
      case other =>
        Left(ParseError(s"Expect comma, got $other"))
    }
  }

  private def parseUntilNth(
      state: ParsingState,
      currentElement: Int,
      targetElement: Int): Either[ParseError, ParsingState] = {

    val newStateE = currentElement match {
      case 0 => parseUntilFirst(state)
      case n => parseUntilNext(state)
    }

    newStateE.flatMap { newState =>
      if (currentElement == targetElement) {
        Right(newState)
      } else {
        parseAnElement(newState).flatMap { parsed =>
          val nextState =
            newState.copy(idx = parsed.endedAt + 1).withCtx(ArrayMid(false))

          parseUntilNth(nextState, currentElement + 1, targetElement)
        }
      }
    }
  }

  def parseArray(
      n: Int): Kleisli[Either[ParseError, ?], ParsingState, ParsingState] =
    Kleisli[Either[ParseError, ?], ParsingState, ParsingState](st =>
      parseUntilNth(st, 0, n))
}
