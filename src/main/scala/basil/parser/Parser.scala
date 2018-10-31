package basil.parser

import cats.effect.IO
import fs2._
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import org.json4s.JsonAST._

object Parser {
  type CharStream    = Stream[IO, Char]
  type Parse[Eff[_]] = Vector[PPath] => CharStream => Eff[(JValue, CharStream)]

  type PerLevelParse = Algebra[ParseOps, Parse[IO]]

  implicit class StreamHelper(s: CharStream) {

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
    def flatSome[O2 >: O, R2](f: R => Pull[IO, O2, R2])(implicit l: sourcecode.Line,
                                                        path: Vector[PPath]): Pull[IO, O2, R2] = {
      p.flatMap {
        case Some(x) => f(x)
        case None    => ParseFailure.termination
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

  private def skipComma(implicit path: Vector[PPath]): StreamConsumer = { stream =>
    stream.pull.uncons1.flatMap {
      case Some((',', next)) => next.pull.echo
      case Some((u, _))      => ParseFailure(",", u.toString, path)
      case None              => ParseFailure.termination
    }.stream
  }

  private def skipNum(expectedTerminator: ExpectedTerminator)(
      implicit path: Vector[PPath]): StreamConsumer = { s =>
    Stream
      .eval(
        parseNumber(expectedTerminator)(path)(s).map(_._2)
      )
      .flatten
  }

  // Does not skip intermediate terminator, eg. `,`
  private def skipOne(term: ExpectedTerminator)(implicit path: Vector[PPath]): StreamConsumer = {
    stream =>
      stream.pull.peek1.flatMap {
        case Some(('"', next))      => skipStr(path)(next).pull.echo
        case Some(('t', next))      => skipBool(path)(next).pull.echo
        case Some(('f', next))      => skipBool(path)(next).pull.echo
        case Some((digit(_), next)) => skipNum(term)(path)(next).pull.echo
        case Some((sign(_), next))  => skipNum(term)(path)(next).pull.echo
        case Some(('[', next))      => skipArr(path)(next).pull.echo
        case Some(('{', next))      => skipObject(path)(next).pull.echo
        case Some((unexp, _))       => ParseFailure("One of(t, f, [, {)", unexp.toString, path)
        case None                   => ParseFailure.termination
      }.stream
  }
  private def skipArr(implicit path: Vector[PPath]): StreamConsumer = { stream =>
    stream.pull.uncons1.flatSome {
      // expect arr to starts with [
      case ('[', next) =>
        next.pull.peek1.flatSome {
          // check if it's empty array
          case (']', next) => next.tail.pull.echo
          case (_, next)   => takeTilArrayEnd(next)(path).pull.echo
        }
      case (ue, _) => ParseFailure("[", ue.toString, path)
    }.stream
  }
  private def takeTilArrayEnd(s: CharStream)(implicit path: Vector[PPath]): CharStream = {
    skipOne(OneOf(List(Bracket, Comma)))(path)(s).pull.peek1.flatSome {
      case (']', next) => next.tail.pull.echo
      case (',', next) => takeTilArrayEnd(next.tail)(path).pull.echo
      case (unexp, _)  => ParseFailure(",", unexp.toString, path)
    }.stream
  }
  private def skipKVPair(implicit path: Vector[PPath]): StreamConsumer = { s =>
    parseObjKey(s).flatMap {
      case (_, next) =>
        skipOne(OneOf(List(Comma, CurlyBrace)))(path)(next).pull.peek1.flatSome {
          case (',', next) => skipKVPair(path)(next.tail).pull.echo
          case ('}', next) => next.pull.echo
          case (uexp, _)   => ParseFailure(", or }", uexp.toString, path)
        }.stream
    }
  }

  private def skipObject(implicit path: Vector[PPath]): StreamConsumer = { s =>
    s.pull.uncons1.flatSome {
      case ('{', next) =>
        skipKVPair(path)(next).pull.uncons1.flatSome {
          case ('}', next) => next.pull.echo
          case (uexp, _)   => ParseFailure("}", uexp.toString, path)
        }
      case (uexp, _) => ParseFailure("{", uexp.toString, path)
    }.stream
  }

  // does not include the terminator
  private def accUntil[A](stream: Stream[IO, A])(until: A => Boolean)(
      implicit path: Vector[PPath]): Stream[IO, (Vector[A], Stream[IO, A])] = {
    def recurse(stream: Stream[IO, A])(acc: Vector[A]): Stream[IO, (Vector[A], Stream[IO, A])] = {
      stream.pull.uncons1.flatMap {
        case Some((a, next)) if until(a) => Pull.output1((acc, next))
        case Some((a, next))             => recurse(next)(acc :+ a).pull.echo
        case None                        => ParseFailure.termination
      }.stream
    }

    recurse(stream)(Vector.empty)
  }

  implicit def perror(e: ParseFailure): Pull[IO, INothing, INothing] = Pull.raiseError[IO](e)

  private def parseObjKey(stream: CharStream)(
      implicit path: Vector[PPath]): Stream[IO, (String, CharStream)] = {

    stream.pull.uncons1.flatSome {
      case ('"', nextStream) =>
        val keyStr = accUntil(nextStream)(_ == '"')

        keyStr.pull.uncons1.flatMap {
          case Some(((key, afterKey), _)) =>
            afterKey.pull.uncons1.flatSome {
              case (':', next) => Pull.output1(key.mkString -> next)
              case (o, _)      => ParseFailure(": for key finding", o.toString, path)
            }
          case None => ParseFailure("Key not found", path)
        }

      case (uexp, _) => ParseFailure("\" to start a key", uexp.toString, path)
    }.stream

  }

  // todo: need to ignore whitespaces
  def parseString: Parse[IO] = { implicit path => stream =>
    stream.pull.uncons1
      .flatSome {
        case ('"', nextStream) => accUntil(nextStream)(_ == '"')(path).pull.echo
        case (other, _)        => ParseFailure(s"""String should starts with ", but found $other""", path)
      }
      .stream
      .head
      .compile
      .lastOrError
      .map {
        case (x, next) => JString(x.mkString) -> next
      }
  }
  def parseBoolean: Parse[IO] = { implicit path => stream =>
    stream.isFollowedBy("true".toCharArray.toList).flatMap { isTrue =>
      if (isTrue) {

        IO.pure(JBool.True -> stream.drop(4))
      } else {
        stream.isFollowedBy("false".toCharArray.toList).flatMap { isFalse =>
          if (isFalse) {
            IO.pure(JBool.False -> stream.drop(5))
          } else {
            IO.raiseError(ParseFailure("Boolean must be true or false", path))
          }
        }
      }
    }
  }

  private def skipStr(implicit path: Vector[PPath]): StreamConsumer =
    s => Stream.eval(parseString(path)(s).map(_._2)).flatten
  private def skipBool(implicit path: Vector[PPath]): StreamConsumer =
    s => Stream.eval(parseBoolean(path)(s).map(_._2)).flatten

  import Pull.output1

  private def parseSign(s: CharStream)(
      implicit path: Vector[PPath]): Stream[IO, (Option[Char], CharStream)] = {
    s.pull.peek1.flatSome {
      case (sign(sChar), next) => Pull.output1(Some(sChar) -> next.tail)
      case (_, next)           => Pull.output1(None        -> next)
    }.stream
  }

  /**
    * return (Vector[Char], Option[Char], CharStream)
    * a - part1 num
    * b - optional terminator char, None indicate no part2
    *     should only have 3 possibilities, Some('.'), Some('e') or None (optimization chance)
    * c - subsequent stream
    */
  private def parseNumPart1(charStream: CharStream)(term: ExpectedTerminator)(
      implicit path: Vector[PPath]): Stream[IO, (Vector[Char], Option[Char], CharStream)] = {

    def recurse(acc: Vector[Char],
                s: CharStream): Stream[IO, (Vector[Char], Option[Char], CharStream)] = {

      s.pull.uncons1.flatFold {
        case (digit(d), nextS)               => recurse(acc :+ d, nextS).pull.echo
        case (t, nextS) if term.matchChar(t) => output1((acc, None, nextS.cons1(t)))
        case (dot, nextS) if dot == '.'      => output1((acc, Some(dot), nextS))
        case (e, nextS) if e == 'e'          => output1((acc, Some(e), nextS))
        case (uexp, _)                       => ParseFailure(s"Digit or $term", uexp.toString, path)
      } {
        if (term == End) {
          Pull.output1((acc, None, Stream.empty))
        } else {
          ParseFailure.termination
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
  private def parseNumPart2(charStream: CharStream, p1Term: Char)(term: ExpectedTerminator)(
      implicit path: Vector[PPath]): Stream[IO, (Vector[Char], Boolean, CharStream)] = {

    def recurse(acc: Vector[Char],
                s: CharStream): Stream[IO, (Vector[Char], Boolean, CharStream)] = {
      s.pull.uncons1.flatFold {
        case (digit(d), nextS)                     => recurse(acc :+ d, nextS).pull.echo
        case (t, nextS) if term.matchChar(t)       => output1((acc, true, nextS.cons1(t)))
        case (exponent(_), nextS) if p1Term == '.' => output1((acc, false, nextS))
        case (uexp, _)                             => ParseFailure(s"Digit or $term", uexp.toString, path)
      } {
        if (term == End) {
          Pull.output1((acc, true, Stream.empty))
        } else {
          ParseFailure.termination
        }
      }.stream
    }

    recurse(Vector.empty, charStream)
  }

  private def parseNumPart3(charStream: CharStream)(term: ExpectedTerminator)(
      implicit path: Vector[PPath]): Stream[IO, (Vector[Char], CharStream)] = {
    def recurse(acc: Vector[Char], s: CharStream): Stream[IO, (Vector[Char], CharStream)] = {
      s.pull.uncons1.flatFold {
        case (digit(d), nextS)              => recurse(acc :+ d, nextS).pull.echo
        case (t, next) if term.matchChar(t) => output1(acc -> next.cons1(t))
        case (uexp, _)                      => ParseFailure(s"Digit or $term", uexp.toString, path)
      } {
        if (term == End) {
          Pull.output1((acc, Stream.empty))
        } else {
          ParseFailure.termination
        }
      }.stream
    }

    parseSign(charStream).flatMap {
      case (Some(s), next) => recurse(Vector(s), next)
      case (None, next)    => recurse(Vector.empty, next)
    }

  }
  def parseNumber(terminator: ExpectedTerminator): Parse[IO] = { implicit path => stream =>
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

  def parseArrayItem(n: Int, next: Parse[IO]): Parse[IO] = { implicit path => stream =>
    def recurse(left: Int): Parse[IO] = { implicit path => stream =>
      if (left == 0) {
        next(path)(stream)
      } else {
        val nMinus1Stream = skipOne(Comma)(path).andThen(skipComma(path))(stream)
        val end           = recurse(left - 1)(path)(nMinus1Stream)
        end
      }
    }

    stream.pull.uncons1
      .flatSome {
        case ('[', nextStream) =>
          val e = recurse(n)(path \ n)(nextStream)
          Stream.eval(e).pull.echo
        case (x, _) => ParseFailure("[", x.toString, path \ n)
      }
      .stream
      .head
      .compile
      .lastOrError
  }
  def parseObj(k: String, nextOp: Parse[IO]): Parse[IO] = { implicit path => stream =>
    def skipUntilKey(implicit path: Vector[PPath]): StreamConsumer = { s =>
      parseObjKey(s).pull.uncons1.flatSome {
        case ((key, nextS), _) if key == k => nextS.pull.echo
        case ((_, nextS), _)               => skipUntilKey(path)(nextS).pull.echo
      }.stream
    }
    if (k.isEmpty) {
      IO.raiseError(ParseFailure("Key not found", "", path \ k))
    } else {
      nextOp(path \ k) {
        stream.pull.uncons1.flatSome {
          case ('{', next) => skipUntilKey(path \ k)(next).pull.echo
          case (c, _)      => ParseFailure("{", c.toString, path)
        }.stream
      }
    }

  }

  def parsing: PerLevelParse = {
    case GetString         => parseString
    case GetBool           => parseBoolean
    case GetNum            => parseNumber(End)
    case GetN(n, next)     => parseArrayItem(n, next)
    case GetKey(key, next) => parseObj(key, next)
    case GetNullable(ops) =>
      path => in =>
        ops(path ?)(in).handleErrorWith { _ =>
          IO.pure(JNull -> in)
        }
  }

  def parse(exp: Fix[ParseOps], stream: CharStream)(implicit path: Vector[PPath]): IO[JValue] = {
    val p: Parse[IO] = exp.cata(parsing)
    p(path)(stream).map(_._1)
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
object exponent {
  def unapply(c: Char): Option[Char] = if (c == 'e' || c == 'E') { Some(c) } else None
}
