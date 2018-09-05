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

  val parseString: Parse[IO] = { stream =>
    stream.pull.uncons1
      .flatMap {
        case Some(('"', nextStream)) =>
          nextStream.pull.takeThrough(_ != '"')
        case Some((other, _)) => Pull.raiseError[IO](PError('\"', Some(other)))
        case None             => Pull.raiseError[IO](PError('\"', None))
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
    def recurse(n: Int, next: Parse[IO]): Parse[IO] = { stream =>
      if (n == 0) {
        next(stream)
      } else {
        {
          val nMinus1Stream = skipOne.andThen(skipComma)(stream)
          val end           = recurse(n - 1, next)(nMinus1Stream)
          end
        }
      }
    }

    stream.pull.uncons1
      .flatMap {
        case Some(('[', nextStream)) =>
          val e = recurse(n, next)(nextStream)
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

  private def takeTilEnd(s: Stream[IO, Char]): Stream[IO, Char] = {
    skipOne(s).pull.peek1.flatMap {
      case Some((']', next)) => next.pull.uncons1
      case Some((',', next)) =>
        takeTilEnd(next.tail).pull.echo
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
          case Some((']', next))   => next.tail.pull.uncons1
          case Some((other, next)) =>
            // wip: pick 1 and take comma, then continue
            takeTilEnd(next).pull.echo
          case None => Pull.raiseError[IO](PError(']', None))
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

  def parseObj(k: String, next: Parse[IO]): Parse[IO] = { wrapper =>
    ???
  }

  def parse(exp: Fix[ParseOps]): Parse[IO] = {
    exp.cata(parsing)
  }

  private def printS[A](s: Stream[IO, A]) = {
    println(s.compile.toList.unsafeRunSync())
  }
}

case class PError(expect: String, received: Option[Char])
    extends Exception(s"Expect [$expect], got $received")

object PError {
  def apply(expect: Char, received: Option[Char]): PError = new PError(expect.toString, received)

  def apply(expect: Char, received: Char): PError = apply(expect, Some(received))
}
