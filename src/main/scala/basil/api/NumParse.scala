package basil.api

import org.json4s.JValue
import org.json4s.JsonAST.JDouble
import scalaz.Kleisli

object NumParse {
  private val numChars: List[Char] = List(
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  )

  private object num {
    def unapply(arg: Char): Option[Char] = {
      numChars.find(_ == arg)
    }
  }
  private object exponent {
    def unapply(arg: Char): Option[Char] =
      if (arg == 'e' || arg == 'E') {
        Some(arg)
      } else {
        None
      }
  }
  private object sign {
    def unapply(arg: Char): Option[Char] = {
      if (arg == '-' || arg == '+') {
        Some(arg)
      } else {
        None
      }
    }
  }

  type FromTo = (Int, Int)

  private def dropLeadingWhiteSpaces(
      state: ParsingState): Either[ParseError, ParsingState] = {
    state.pointedChar match {
      case whitespaces(_) => dropLeadingWhiteSpaces(state.idxIncrement(1))
      case num(_)         => Right(state)
      case other =>
        Left(
          ParseError(
            s"Unexpected $other found when dropping leading white space"))
    }
  }

  private def getSign(state: ParsingState): Either[ParseError, ParsingState] = {
    state.pointedChar match {
      case '-'    => Right(state.idxIncrement(1))
      case num(_) => Right(state)
      case other =>
        Left(ParseError(s"Unexpected char $other when parsing number"))
    }
  }

  private def parseAfterSign(
      state: ParsingState,
      numParseState: NumParseState): Either[ParseError, ParsingState] = {
    (state.ctx, state.pointedChar, numParseState) match {
      case (RootC, num(_), _) if state.isLast => Right(state)
      case (_, num(_), _) =>
        parseAfterSign(state.idxIncrement(1), numParseState)

      case (_, '.', ps) if ps != NumDotted && ps != NumExponented =>
        parseAfterSign(state.idxIncrement(1), NumDotted)

      case (_, exponent(_), ps) if ps != NumExponented =>
        state.charAt(state.idx + 1) match {
          case sign(_) =>
            parseAfterSign(state.idxIncrement(2), NumExponented)
          case _ =>
            parseAfterSign(state.idxIncrement(1), NumExponented)
        }

      case (ArrayC, ',', _) => Right(state)
      case (ArrayC, ']', _) => Right(state)
      case (ObjC, ',', _)   => Right(state)
      case (ObjC, '}', _)   => Right(state)
    }
  }

  private def parseNumRecurse(
      state: ParsingState): Either[ParseError, FromTo] = {

    for {
      noWs <- dropLeadingWhiteSpaces(state)
      noSign <- getSign(noWs)
      result <- parseAfterSign(noSign, NumStart)
    } yield {
      (noWs.idx, result.idx)
    }
  }

  val parseNum: ParseAction[Parsed[JValue]] =
    Kleisli[ParseResult, ParsingState, Parsed[JValue]](
      state =>
        parseNumRecurse(state)
          .map {
            case (from, to) =>
              Parsed(to,
                     JDouble(state.raw.slice(from, to + 1).mkString.toDouble))
        }
    )
}

private sealed trait NumParseState
private case object NumStart extends NumParseState
private case object NumDotted extends NumParseState
private case object NumExponented extends NumParseState
