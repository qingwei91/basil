package basil

import cats.effect._
import fs2._
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import org.json4s.JsonAST._

package object parser {
  type CharStream    = Stream[IO, Char]
  type Parse[Eff[_]] = Stream[IO, Char] => Eff[JValue]

  type PerLevelParse = Algebra[ParseOps, Parse[IO]]

  val parsing: PerLevelParse = {
    case GetString         => parseString
    case GetBool           => parseBoolean
    case GetNum            => parseNumber(End)
    case GetN(n, next)     => parseArrayItem(n, next)
    case GetKey(key, next) => parseObj(key, next)
    case GetNullable(ops)  => ops
  }

  implicit class StreamHelper(s: Stream[IO, Char]) {
    def dropExpected(expectation: List[Char]): Stream[IO, Char] = {
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

  type StreamConsumer = Stream[IO, Char] => Stream[IO, Char]

  private val skipComma: StreamConsumer = { stream =>
    stream.pull.uncons1.flatMap {
      case Some((',', next)) => next.pull.echo
      case Some((u, _))      => Pull.raiseError[IO](PError(',', u))
      case None              => Pull.raiseError[IO](PError(',', None))
    }.stream
  }
  private val skipStr: StreamConsumer = s => s.drop(1).dropThrough(_ != '"')
  private def skipBool(b: Boolean): StreamConsumer = s => {
    val boolStr = if (b) "true" else "false"
    s.dropExpected(boolStr.toCharArray.toList)
  }

  private def skipOne: StreamConsumer = { stream =>
    stream.pull.peek1.flatMap {
      case Some(('"', next))      => skipStr(next).pull.echo
      case Some(('t', next))      => skipBool(true)(next).pull.echo
      case Some(('f', next))      => skipBool(false)(next).pull.echo
      case Some((digit(_), next)) => ??? // todo: skip number
      case Some(('[', next))      => skipArr(next).pull.echo
      case Some(('{', _))         => ??? // todo: skip object
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

  private def takeTilArrayEnd(s: Stream[IO, Char]): Stream[IO, Char] = {
    skipOne(s).pull.peek1.flatSome {
      case (']', next) => next.tail.pull.echo
      case (',', next) => takeTilArrayEnd(next.tail).pull.echo
      case (unexp, _)  => Pull.raiseError[IO](PError(',', Some(unexp)))
    }.stream
  }

  private object digit {
    private val digits = (0 to 9).map(x => Character.forDigit(x, 10))
    def unapply(arg: Char): Option[Char] = {
      Some(arg).filter(digits.contains)
    }
    def notDigit(char: Char): Boolean = !digits.contains(char)
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

  private def parseObjKey(stream: Stream[IO, Char]): Stream[IO, (String, Stream[IO, Char])] = {

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
          // todo: this does not ensure string complete
          nextStream.pull.takeThrough(_ != '"')
        case Some((other, _)) =>
          Pull.raiseError[IO](PError('\"', Some(other)))
        case None => Pull.raiseError[IO](PError('\"', None))
      }
      .stream
      .compile
      .toVector
      .map(x => JString(x.dropRight(1).mkString))
  }
  val parseBoolean: Parse[IO] = { stream =>
    stream.isFollowedBy("true".toCharArray.toList).flatMap { isTrue =>
      if (isTrue) {
        IO.pure(JBool.True)
      } else {
        stream.isFollowedBy("false".toCharArray.toList).flatMap { isFalse =>
          if (isFalse) IO.pure(JBool.False) else IO.raiseError(PError("True or false", None))
        }
      }
    }
  }

  import Pull.output1
  private def parseNumPart1(charStream: CharStream)(
      term: ExpectedTerminator): Stream[IO, (Vector[Char], Option[(Char, CharStream)])] = {
    def parseSign(s: CharStream): Stream[IO, (Option[Char], CharStream)] = {
      s.pull.peek1.flatSome {
        case (sign(sChar), next) => Pull.output1(Some(sChar) -> next.tail)
        case (_, next)           => Pull.output1(None        -> next)
      }.stream
    }

    def recurse(acc: Vector[Char],
                s: CharStream): Stream[IO, (Vector[Char], Option[(Char, CharStream)])] = {

      s.pull.uncons1.flatFold {
        case (digit(d), nextS)           => recurse(acc :+ d, nextS).pull.echo
        case (t, _) if term.matchChar(t) => output1(acc -> None)
        case (dot, nextS) if dot == '.'  => output1(acc -> Some(dot -> nextS))
        case (e, nextS) if e == 'e'      => output1(acc -> Some(e -> nextS))
        case (uexp, _)                   => PError(s"Digit or $term", uexp)
      } {
        if (term == End) {
          Pull.output1(acc -> None)
        } else {
          PError("Unexpected termination", None)
        }
      }.stream
    }

    parseSign(charStream).flatMap {
      case (Some(s), next) => recurse(Vector(s), next)
      case (None, next)    => recurse(Vector.empty, next)
    }

    /**
    * - Terminated: Vector[Char]
    * - (Vector[Char], '.', CharStream)
    * - (Vector[Char], 'e', CharStream)
    * - Unexpected char
    * - Unexpected end of char
    */
  }

  private def parseNumPart2(charStream: CharStream, p1Term: Char)(
      term: ExpectedTerminator): Stream[IO, (Vector[Char], Option[CharStream])] = {
    def recurse(acc: Vector[Char],
                s: CharStream): Stream[IO, (Vector[Char], Option[CharStream])] = {
      s.pull.uncons1.flatFold {
        case (digit(d), nextS)                       => recurse(acc :+ d, nextS).pull.echo
        case (t, _) if term.matchChar(t)             => output1(acc -> None)
        case (e, nextS) if e == 'e' && p1Term == '.' => output1(acc -> Some(nextS))
        case (uexp, _)                               => PError(s"Digit or $term", uexp)
      } {
        if (term == End) {
          Pull.output1(acc -> None)
        } else {
          PError("Unexpected termination", None)
        }
      }.stream
    }

    recurse(Vector.empty, charStream)
  }

  private def parseNumPart3(charStream: CharStream)(
      term: ExpectedTerminator): Stream[IO, Vector[Char]] = {
    def recurse(acc: Vector[Char], s: CharStream): Stream[IO, Vector[Char]] = {
      s.pull.uncons1.flatSome {
        case (digit(d), nextS)           => recurse(acc :+ d, nextS).pull.echo
        case (t, _) if term.matchChar(t) => output1(acc)
        case (uexp, _)                   => PError(s"Digit or $term", uexp)
      }.stream
    }

    recurse(Vector.empty, charStream)
  }

  def parseNumber(terminator: ExpectedTerminator): Parse[IO] = { stream =>
    parseNumPart1(stream)(terminator)
      .flatMap {
        case (part1, Some((p1T, next))) =>
          parseNumPart2(next, p1T)(terminator).flatMap {
            case (part2, Some(next)) =>
              parseNumPart3(next)(terminator)
                .flatMap { part3 =>
                  Stream.apply(part1.:+(p1T).++(part2).:+('e').++(part3))
                }
            case (part2, None) => Stream.apply(part1.:+(p1T).++(part2))
          }
        case (part1, None) => Stream.apply(part1)
      }
      .head
      .compile
      .lastOrError
      .map { numChars =>
        JDouble(numChars.mkString.toDouble)
      }
  }
  def parseArrayItem(n: Int, next: Parse[IO]): Parse[IO] = { stream =>
    def recurse(n: Int): Parse[IO] = { stream =>
      if (n == 0) {
        next(stream)
      } else {
        {
          val nMinus1Stream = skipOne.andThen(skipComma)(stream)
          val end           = recurse(n - 1)(nMinus1Stream)
          end
        }
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
      .toVector
      .map(_.head)

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

  def parse(exp: Fix[ParseOps]): Parse[IO] = {
    exp.cata(parsing)
  }
}
