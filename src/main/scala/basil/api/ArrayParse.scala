package basil.api

import org.json4s.JValue
import scalaz.{Bind, Kleisli}

import scala.annotation.tailrec

class ArrayParse(stringParser: StringParse) {
  import stringParser._

  implicit val eitherBind: Bind[ParseResult] = new Bind[ParseResult] {
    override def bind[A, B](fa: ParseResult[A])(f: A => ParseResult[B]): ParseResult[B] = {
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

  private def parseAnElement: ParseAction[Parsed[JValue]] = ???

  private def skipArray: ParseAction[Parsed[JValue]] = Kleisli { state =>
    parseUntilFirst(state).flatMap(parseUntilEnd).map(s => Parsed(s.idx, ???))
    ???
  }

  private def parseUntilEnd(state: ParsingState): Either[ParseError, ParsingState] = {
    parseUntilNext(state, ArrayMidBeforeComma).flatMap { st =>
      st.pointedChar match {
        case ']' => Right(st.idxIncrement(1))
        case _   => parseUntilEnd(state)
      }
    }
  }

  private def parseUntilFirst(
      state: ParsingState,
      openBracketSeen: Boolean = false): Either[ParseError, ParsingState] = {

    state.pointedChar match {
      case whitespaces(_) =>
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

  private def parseUntilNext(state: ParsingState,
                             arrParseState: ArrayParseState): Either[ParseError, ParsingState] = {

    (state.pointedChar, arrParseState) match {
      case (whitespaces(_), _) =>
        parseUntilNext(state.idxIncrement(1), arrParseState)

      case (',', ArrayMidAfterComma) =>
        Left(ParseError(s"Syntax error, multiple comma: $state"))

      case (',', ArrayMidBeforeComma) =>
        parseUntilNext(state.idxIncrement(1), ArrayMidAfterComma)

      case (_, ArrayMidAfterComma) => Right(state)

      case other =>
        Left(ParseError(s"Expect comma, got $other"))
    }
  }

  @tailrec
  private def parseUntilNth(state: ParsingState,
                            currentElement: Int,
                            targetElement: Int): Either[ParseError, ParsingState] = {

    val newStateE = currentElement match {
      case 0 => parseUntilFirst(state)
      case n => parseUntilNext(state, ArrayMidBeforeComma)
    }

    newStateE match {
      case Right(newState) =>
        if (currentElement == targetElement) {
          Right(newState)
        } else {
          parseAnElement(newState) match {
            case Right(parsed) =>
              val nextState = newState.copy(idx = parsed.endedAt + 1)

              parseUntilNth(nextState, currentElement + 1, targetElement)

            case Left(left) => Left(left)
          }
        }
      case left => left
    }
  }

  def parseArray(n: Int): Kleisli[Either[ParseError, ?], ParsingState, ParsingState] =
    Kleisli[Either[ParseError, ?], ParsingState, ParsingState](st => parseUntilNth(st, 0, n))
}

sealed trait ArrayParseState
case object ArrayMidBeforeComma extends ArrayParseState
case object ArrayMidAfterComma  extends ArrayParseState
