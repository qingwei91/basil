package basil.parser

import cats.effect.IO
import fs2._
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import org.json4s.JsonAST._

object Parser {
  type CharStream    = Stream[IO, Char]
  type Parse[Eff[_]] = CharStream => Eff[(JValue, CharStream)]

  type PerLevelParse = Algebra[ParseOps, Parse[IO]]

  implicit class StreamHelper(s: CharStream) {
    def dropExpected(expectation: List[Char]): CharStream = {
      expectation match {
        case h :: t =>
          s.pull.uncons1.flatMap {
            case Some((c, next)) if c == h => next.dropExpected(t).pull.echo
            case Some((c, _))              => PError(h, c)
            case None                      => PError(h, None)
          }.stream
        case Nil => s
      }
    }

    def isFollowedBy(expect: List[Char]): IO[Boolean] = {
      expect match {
        case h :: t =>
          s.pull.uncons1
            .flatMap {
              case Some((c, next)) if c == h =>
                Stream.eval(next.isFollowedBy(t)).pull.echo
              case _ => Pull.output1(false)
            }
            .stream
            .head
            .compile
            .lastOrError
        case Nil => IO.pure(true)
      }
    }
  }
  implicit class PullHelper[O, R](p: Pull[IO, O, Option[R]]) {
    def flatSome[O2 >: O, R2](f: R => Pull[IO, O2, R2])(
        implicit l: sourcecode.Line): Pull[IO, O2, R2] = {
      p.flatMap {
        case Some(x) => f(x)
        case None    => PError("Unexpected termination", None)
      }
    }
    def flatFold[O2 >: O, R2](f: R => Pull[IO, O2, R2])(
        orElse: Pull[IO, O2, R2]): Pull[IO, O2, R2] = {
      p.flatMap {
        case Some(x) => f(x)
        case None    => orElse
      }
    }
  }

  type StreamConsumer = CharStream => CharStream

  private val skipComma: StreamConsumer = { stream =>
    stream.pull.uncons1.flatMap {
      case Some((',', next)) => next.pull.echo
      case Some((u, _))      => PError(',', u)
      case None              => PError(',', None)
    }.stream
  }

  private def skipNum(expectedTerminator: ExpectedTerminator): StreamConsumer = { s =>
    Stream
      .eval(
        parseNumber(expectedTerminator)(s).map(_._2)
      )
      .flatten
  }

  // Does not skip intermediate terminator, eg. `,`
  private def skipOne(term: ExpectedTerminator): StreamConsumer = { stream =>
    stream.pull.peek1.flatMap {
      case Some(('"', next))      => skipStr(next).pull.echo
      case Some(('t', next))      => skipBool(next).pull.echo
      case Some(('f', next))      => skipBool(next).pull.echo
      case Some((digit(_), next)) => skipNum(term)(next).pull.echo
      case Some((sign(_), next))  => skipNum(term)(next).pull.echo
      case Some(('[', next))      => skipArr(next).pull.echo
      case Some(('{', next))      => skipObject(next).pull.echo
      case Some((unexp, _))       => PError("One of(t, f, [, {)", unexp)
      case None                   => PError("One of(t, f, [, {)", None)
    }.stream
  }
  private def skipArr: StreamConsumer = { stream =>
    stream.pull.uncons1.flatSome {
      // expect arr to starts with [
      case ('[', next) =>
        next.pull.peek1.flatSome {
          // check if it's empty array
          case (']', next) => next.tail.pull.echo
          case (_, next)   => takeTilArrayEnd(next).pull.echo
        }
      case (ue, _) => PError('[', Some(ue))
    }.stream
  }
  private def takeTilArrayEnd(s: CharStream): CharStream = {
    skipOne(OneOf(List(Bracket, Comma)))(s).pull.peek1.flatSome {
      case (']', next) => next.tail.pull.echo
      case (',', next) => takeTilArrayEnd(next.tail).pull.echo
      case (unexp, _)  => Pull.raiseError[IO](PError(',', Some(unexp)))
    }.stream
  }
  private def skipKVPair: StreamConsumer = { s =>
    parseObjKey(s).flatMap {
      case (_, next) =>
        skipOne(OneOf(List(Comma, CurlyBrace)))(next).pull.peek1.flatSome {
          case (',', next) => skipKVPair(next.tail).pull.echo
          case ('}', next) => next.tail.pull.echo
          case (uexp, _)   => PError(", or }", uexp)
        }.stream
    }
  }

  private def skipObject: StreamConsumer = { s =>
    s.pull.uncons1.flatSome {
      case ('{', next) =>
        skipKVPair(next).pull.uncons1.flatSome {
          case ('}', next) => next.pull.echo
          case (uexp, _)   => PError('}', uexp)
        }
      case (uexp, _) => PError('{', uexp)
    }.stream
  }

  // does not include the terminator
  private def accUntil[A](stream: Stream[IO, A])(
      until: A => Boolean): Stream[IO, (Vector[A], Stream[IO, A])] = {
    def recurse(stream: Stream[IO, A])(acc: Vector[A]): Stream[IO, (Vector[A], Stream[IO, A])] = {
      stream.pull.uncons1.flatMap {
        case Some((a, next)) if until(a) =>
          Pull.output1((acc, next))
        case Some((a, next)) =>
          recurse(next)(acc :+ a).pull.echo
        case None =>
          Pull.raiseError[IO](PError("Condition not met", None))
      }.stream
    }

    recurse(stream)(Vector.empty)
  }

  implicit def perror(e: PError): Pull[IO, INothing, INothing] = Pull.raiseError[IO](e)

  private def parseObjKey(stream: CharStream): Stream[IO, (String, CharStream)] = {

    stream.pull.uncons1.flatMap {
      case Some(('"', nextStream)) =>
        val keyStr = accUntil(nextStream)(_ == '"')

        keyStr.pull.uncons1.flatMap {
          case Some(((key, afterKey), _)) =>
            afterKey.pull.uncons1.flatSome {
              case (':', next) => Pull.output1(key.mkString -> next)
              case (o, _)      => PError(':', o)
            }
          case None => PError("No Key", None)
        }

      case other => PError('"', other.map(_._1))
    }.stream

  }

  // todo: need to ignore whitespaces
  val parseString: Parse[IO] = { stream =>
    stream.pull.uncons1
      .flatMap {
        case Some(('"', nextStream)) =>
          accUntil(nextStream)(_ == '"').pull.echo
        case Some((other, _)) => PError('\"', Some(other))
        case None             => PError('\"', None)
      }
      .stream
      .head
      .compile
      .lastOrError
      .map {
        case (x, next) => JString(x.mkString) -> next
      }
  }
  val parseBoolean: Parse[IO] = { stream =>
    stream.isFollowedBy("true".toCharArray.toList).flatMap { isTrue =>
      if (isTrue) {

        IO.pure(JBool.True -> stream.drop(4))
      } else {
        stream.isFollowedBy("false".toCharArray.toList).flatMap { isFalse =>
          if (isFalse) {
            IO.pure(JBool.False -> stream.drop(5))
          } else {
            IO.raiseError(PError("True or false", None))
          }
        }
      }
    }
  }

  private val skipStr: StreamConsumer  = s => Stream.eval(parseString(s).map(_._2)).flatten
  private val skipBool: StreamConsumer = s => Stream.eval(parseBoolean(s).map(_._2)).flatten

  import Pull.output1

  /**
    * return (Vector[Char], Option[Char], CharStream)
    * a - part1 num
    * b - optional terminator char, None indicate no part2
    *     should only have 3 possibilities, Some('.'), Some('e') or None (optimization chance)
    * c - subsequent stream
    */
  private def parseNumPart1(charStream: CharStream)(
      term: ExpectedTerminator): Stream[IO, (Vector[Char], Option[Char], CharStream)] = {
    def parseSign(s: CharStream): Stream[IO, (Option[Char], CharStream)] = {
      s.pull.peek1.flatSome {
        case (sign(sChar), next) => Pull.output1(Some(sChar) -> next.tail)
        case (_, next)           => Pull.output1(None        -> next)
      }.stream
    }

    def recurse(acc: Vector[Char],
                s: CharStream): Stream[IO, (Vector[Char], Option[Char], CharStream)] = {

      s.pull.uncons1.flatFold {
        case (digit(d), nextS)               => recurse(acc :+ d, nextS).pull.echo
        case (t, nextS) if term.matchChar(t) => output1((acc, None, nextS))
        case (dot, nextS) if dot == '.'      => output1((acc, Some(dot), nextS))
        case (e, nextS) if e == 'e'          => output1((acc, Some(e), nextS))
        case (uexp, _)                       => PError(s"Digit or $term", uexp)
      } {
        if (term == End) {
          Pull.output1((acc, None, Stream.empty))
        } else {
          PError("Unexpected termination", None)
        }
      }.stream
    }

    parseSign(charStream).flatMap {
      case (Some(s), next) => recurse(Vector(s), next)
      case (None, next)    => recurse(Vector.empty, next)
    }
  }

  /**
    * (Vector[Char], Boolean, CharStream)
    * a - part2 num
    * b - true is terminated
    * c - subsequent stream
    */
  private def parseNumPart2(charStream: CharStream, p1Term: Char)(
      term: ExpectedTerminator): Stream[IO, (Vector[Char], Boolean, CharStream)] = {

    def recurse(acc: Vector[Char],
                s: CharStream): Stream[IO, (Vector[Char], Boolean, CharStream)] = {
      s.pull.uncons1.flatFold {
        case (digit(d), nextS)                       => recurse(acc :+ d, nextS).pull.echo
        case (t, nextS) if term.matchChar(t)         => output1((acc, true, nextS))
        case (e, nextS) if e == 'e' && p1Term == '.' => output1((acc, false, nextS))
        case (uexp, _)                               => PError(s"Digit or $term", uexp)
      } {
        if (term == End) {
          Pull.output1((acc, true, Stream.empty))
        } else {
          PError("Unexpected termination", None)
        }
      }.stream
    }

    recurse(Vector.empty, charStream)
  }

  private def parseNumPart3(charStream: CharStream)(
      term: ExpectedTerminator): Stream[IO, (Vector[Char], CharStream)] = {
    def recurse(acc: Vector[Char], s: CharStream): Stream[IO, (Vector[Char], CharStream)] = {
      s.pull.uncons1.flatSome {
        case (digit(d), nextS)              => recurse(acc :+ d, nextS).pull.echo
        case (t, next) if term.matchChar(t) => output1(acc -> next)
        case (uexp, _)                      => PError(s"Digit or $term", uexp)
      }.stream
    }

    recurse(Vector.empty, charStream)
  }
  def parseNumber(terminator: ExpectedTerminator): Parse[IO] = { stream =>
    parseNumPart1(stream)(terminator)
      .flatMap {
        case (part1, Some(p1T), next) =>
          parseNumPart2(next, p1T)(terminator).flatMap {
            case (part2, false, next) => // not terminated
              parseNumPart3(next)(terminator)
                .flatMap {
                  case (part3, next) =>
                    val fullStr = (part1 :+ p1T) ++ (part2 :+ 'e') ++ part3
                    Stream.apply(fullStr -> next)
                }
            case (part2, true, next) => // terminated
              val combined = (part1 :+ p1T) ++ part2
              Stream.apply(combined -> next)
          }
        case (part1, None, next) => Stream.apply(part1 -> next)
      }
      .head
      .compile
      .lastOrError
      .map {
        case (numChars, next) => JDouble(numChars.mkString.toDouble) -> next
      }
  }
  def parseArrayItem(n: Int, next: Parse[IO]): Parse[IO] = { stream =>
    def recurse(n: Int): Parse[IO] = { stream =>
      if (n == 0) {
        next(stream)
      } else {
        val nMinus1Stream = skipOne(Comma).andThen(skipComma)(stream)
        val end           = recurse(n - 1)(nMinus1Stream)
        end
      }
    }

    stream.pull.uncons1
      .flatMap {
        case Some(('[', nextStream)) =>
          val e = recurse(n)(nextStream)
          Stream.eval(e).pull.echo
        case Some((x, _)) => Pull.raiseError[IO](PError('[', Some(x)))
        case None         => Pull.raiseError[IO](PError('[', None))
      }
      .stream
      .head
      .compile
      .lastOrError
  }
  def parseObj(k: String, nextOp: Parse[IO]): Parse[IO] = { stream =>
    def skipUntilKey: StreamConsumer = { s =>
      parseObjKey(s).pull.uncons1.flatSome {
        case ((key, nextS), _) if key == k => nextS.pull.echo
        case ((_, nextS), _)               => skipUntilKey(nextS).pull.echo
      }.stream
    }

    nextOp {
      stream.pull.uncons1.flatSome {
        case ('{', next) => skipUntilKey(next).pull.echo
        case (c, _)      => PError('{', c)
      }.stream
    }
  }

  val parsing: PerLevelParse = {
    case GetString         => parseString
    case GetBool           => parseBoolean
    case GetNum            => parseNumber(End)
    case GetN(n, next)     => parseArrayItem(n, next)
    case GetKey(key, next) => parseObj(key, next)
    case GetNullable(ops)  => ops
  }

  def parse(exp: Fix[ParseOps]): Parse[IO] = {
    exp.cata(parsing)
  }
}

object digit {
  private val digits = (0 to 9).map(x => Character.forDigit(x, 10))
  def unapply(arg: Char): Option[Char] = {
    Some(arg).filter(digits.contains)
  }
  def notDigit(char: Char): Boolean = !digits.contains(char)
}
object sign {
  def unapply(arg: Char): Option[Char] = {
    if (arg == '-' || arg == '+') {
      Some(arg)
    } else {
      None
    }
  }
}
