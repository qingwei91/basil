package basil

import basil.parser.ParseOps
import cats.effect._
import cats.effect.implicits._
import fs2._
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._
import org.json4s.JsonAST._

package object parser {
  type Parse[Eff[_]] = Stream[IO, Char] => Eff[JValue]

  type PerLevelParse = Algebra[ParseOps, Parse[IO]]

  val parsing: PerLevelParse = {
    case GetString         => parseString
    case GetN(n, next)     => parseArrayItem(n, next)
    case GetKey(key, next) => parseObj(key, next)
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

  val skipComma: StreamConsumer = { stream =>
    stream.pull.uncons1.flatMap {
      case Some((',', next)) => next.pull.echo
      case Some((u, next))   => Pull.raiseError[IO](PError(',', u))
      case None              => Pull.raiseError[IO](PError(',', None))
    }.stream
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

  implicit class StreamHelper(s: Stream[IO, Char]) {
    def consumeWithExpectation(expectation: List[Char]): Stream[IO, Char] = {
      expectation match {
        case h :: t =>
          s.pull.uncons1.flatMap {
            case Some((c, next)) if c == h =>
              next.consumeWithExpectation(t).pull.peek
            case None => Pull.raiseError[IO](PError(h, None))
          }.stream
        case Nil => s
      }

    }
  }

  type StreamConsumer = Stream[IO, Char] => Stream[IO, Char]

  def skipOne: StreamConsumer = { stream =>
    stream.pull.peek1.flatMap {
      case Some(('"', next)) =>
        next.drop(1).dropThrough(_ != '"').pull.echo
      case Some(('t', next))     => next.consumeWithExpectation("true".toCharArray.toList).pull.peek
      case Some(('f', next))     => next.consumeWithExpectation("false".toCharArray.toList).pull.peek
      case Some((digit(), next)) => next.pull.takeWhile(digit.unapply)
      case Some(('[', next))     => skipArr(next).pull.echo
      case Some(('{', next))     => ??? // todo: skip object
      case Some((unexp, next))   => Pull.raiseError[IO](PError("One of(t, f, [, {)", Some(unexp)))
      case None                  => Pull.raiseError[IO](PError("One of(t, f, [, {)", None))
    }.stream
  }

  private def takeTilArrayEnd(s: Stream[IO, Char]): Stream[IO, Char] = {
    skipOne(s).pull.peek1.flatMap {
      case Some((']', next)) => next.tail.pull.echo
      case Some((',', next)) =>
        takeTilArrayEnd(next.tail).pull.echo
      case Some((unexp, next)) => Pull.raiseError[IO](PError(',', Some(unexp)))
      case None                => Pull.raiseError[IO](PError(',', None))
    }.stream
  }

  def skipArr: StreamConsumer = { stream =>
    stream.pull.uncons1.flatMap {
      // expect arr to starts with [
      case Some(('[', next)) =>
        next.pull.peek1.flatMap {
          // check if it's empty array
          case Some((']', next))   => next.tail.pull.echo
          case Some((other, next)) => takeTilArrayEnd(next).pull.echo
          case None                => Pull.raiseError[IO](PError(']', None))
        }
      case Some((ue, next)) =>
        Pull.raiseError[IO](PError('[', Some(ue)))
      case None =>
        Pull.raiseError[IO](PError('[', None))
    }.stream
  }

  object digit {
    val digits = (0 to 9).map(_.toChar)
    def unapply(arg: Char): Boolean = {
      digits.contains(arg)
    }
  }

  def accUntil[A](stream: Stream[IO, A])(
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

  def parseObjKey(stream: Stream[IO, Char]): Stream[IO, (String, Stream[IO, Char])] = {

    stream.pull.uncons1.flatMap {
      case Some(('"', nextStream)) =>
        val keyStr = accUntil(nextStream)(_ == '"')

        keyStr.pull.uncons1.flatMap {
          case Some(((key, afterKey), _)) =>
            afterKey.pull.uncons1.flatMap {
              case Some((':', next)) => Pull.output1(key.mkString -> next)
              case Some((o, next))   => PError(':', o)
              case None              => PError(':', None)
            }
          case None => PError("No Key", None)
        }

      case other => PError('"', other.map(_._1))
    }.stream

  }

  def parseObj(k: String, nextOp: Parse[IO]): Parse[IO] = { stream =>
    def skipUntilKey: StreamConsumer = { s =>
      parseObjKey(s).pull.uncons1.flatMap {
        case Some(((key, nextS), _)) if key == k =>
          nextS.pull.echo
        case Some(((key, nextS), _)) =>
          skipUntilKey(nextS).pull.echo
        case None =>
          Pull.raiseError[IO](PError("Unexpected termination", None))
      }.stream
    }

    nextOp {
      stream.pull.uncons1.flatMap {
        case Some(('{', next)) => skipUntilKey(next).pull.echo
      }.stream
    }
  }

  def parse(exp: Fix[ParseOps]): Parse[IO] = {
    exp.cata(parsing)
  }

  private def printS[A](s: Stream[IO, A]) = {
    println(s.compile.toList.unsafeRunSync())
  }
}

case class PError(expect: String, received: Option[Char])(implicit l: sourcecode.Line)
    extends Exception(s"Expect [$expect], got $received at line: $l")

object PError {
  def apply(expect: Char, received: Option[Char])(implicit l: sourcecode.Line): PError =
    new PError(expect.toString, received)

  def apply(expect: Char, received: Char)(implicit l: sourcecode.Line): PError =
    apply(expect, Some(received))
}
