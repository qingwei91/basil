package basil.api

import org.json4s.{JString, JValue}
import scalaz.Kleisli

trait StringParse {
  private val escapedChars = Seq('b', 'f', 'n', 'r', 't', '"', '/', '\\')

  private def readEscapedChar(escapedChar: Char): Either[ParseError, Char] = {
    escapedChar match {
      case 'b'  => Right('\b')
      case 'f'  => Right('\f')
      case 'n'  => Right('\n')
      case 'r'  => Right('\r')
      case 't'  => Right('\t')
      case '"'  => Right('"')
      case '/'  => Right('/')
      case '\\' => Right('\\')
      case a    => Left(ParseError(s"Expect one of $escapedChars, got $a"))
    }
  }

  val parseString: ParseAction[Parsed[JValue]] =
    Kleisli[ParseResult, ParsingState, Parsed[JValue]](
      ctx =>
        parseStringRHelper(ctx.raw,
                           ctx.idx,
                           ctx.idx,
                           Right(Parsed(ctx.idx, "")))
          .map(s => s.copy(value = JString(s.value)))
    )

  private def parseStringRHelper(raw: String,
                                 from: Int,
                                 to: Int,
                                 acc: Either[ParseError, Parsed[String]])
    : Either[ParseError, Parsed[String]] = {
    acc match {
      case Right(ac) =>
        if (raw.length <= to) {
          Left(ParseError("Malformed JSON, parsing String, reach EOF"))
        } else {
          val nextChar = raw.charAt(to)

          nextChar match {
            case '\\' =>
              val escaped =
                readEscapedChar(raw.charAt(to + 1)).map { c =>
                  val Parsed(acTo, accStr) = ac
                  ac.copy(endedAt = acTo + 2, value = accStr + c)
                }

              parseStringRHelper(raw, from, to + 2, escaped)
            case '"' if ac.value.isEmpty =>
              parseStringRHelper(raw, from, to + 1, Right(Parsed(to + 1, "")))
            case '"' =>
              Right(ac)
            case n =>
              parseStringRHelper(
                raw,
                from,
                to + 1,
                Right(ac.copy(endedAt = to + 1, value = ac.value + n)))
          }
        }
      case left => left
    }
  }
}

object StringParse extends StringParse
