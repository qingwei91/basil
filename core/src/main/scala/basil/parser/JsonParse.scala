package basil.parser

import basil.data._
import basil.typeclass.TakeOne._
import basil.typeclass.{Cons, TakeOne}
import cats.data.NonEmptyList
import cats.free.FreeApplicative
import cats.instances.char._
import cats.kernel.Monoid
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, MonadError, ~>}

/**
  * Core abstraction of the library
  * Contains logic to extract data from json
  *
  * The main selling point is that it works on 1 character at a time
  * and thus can work on partial json and stream of json
  *
  * By trying to extract the exact data needed, it avoids creating
  * intermediate data structure
  */
abstract class JsonParse[Source[_]](implicit TakeOne: TakeOne[Source],
                                    ME: MonadError[Source, ParseFailure],
                                    Monoid: Monoid[Source[Char]],
                                    Cons: Cons[Source]) {

  type CharSource = Source[Char]

  type Parse[I] = Vector[PPath] => CharSource => Source[(I, CharSource)]

  type Pipe = CharSource => CharSource

  val parseString: Parse[String] = { implicit path => src =>
    skipWS(src).take1
      .flatMap {
        case ('"', next) => accJsString(next)
        case (other, _) =>
          ME.raiseError[(Vector[Char], Source[Char])](
            ParseFailure(s"""String should starts with ", but found $other""", path))
      }
      .map {
        case (acc, next) => acc.mkString -> next
      }
  }
  val parseBoolean: Parse[Boolean] = { implicit path => src =>
    val withoutWS = skipWS(src)
    withoutWS.isFollowedBy("true".toCharArray.toList).flatMap {
      case (isTrue, next) =>
        if (isTrue) {
          ME.pure(true -> next)
        } else {
          withoutWS.isFollowedBy("false".toCharArray.toList).flatMap {
            case (isFalse, next) =>
              if (isFalse) {
                ME.pure(false -> next)
              } else {
                ME.raiseError(ParseFailure("Expecting either `true` or `false`", path))
              }
          }
        }
    }
  }

  /**
    * Number is broken down into 3 parts
    *
    * 20.6e10
    *
    * part 1 - 20 terminated by .
    * part 2 - 6 terminated by e
    * part 3 - 10 terminated by terminator
    *
    * @param terminator to indicate what character terminates the number
    * @return
    */
  def parseNumber(terminator: ExpectedTerminator): Parse[Double] = { implicit path => s =>
    parseNum1(skipWS(s))(terminator)
      .flatMap {
        case Part1(p1, Some(sep1), next) =>
          parseNum2(next, sep1)(terminator).flatMap {
            case Part2(p2, Some(sep2), next) =>
              parseNum3(next)(terminator).flatMap {
                case (p3, next) =>
                  val full = (p1 :+ sep1) ++ (p2 :+ sep2) ++ p3
                  ME.pure(full -> next)
              }
            case Part2(p2, None, next) =>
              val full = (p1 :+ sep1) ++ p2
              ME.pure(full -> next)
          }
        case Part1(p1, None, next) => ME.pure(p1 -> next)
      }
      .map {
        case (numChars, next) => numChars.mkString.toDouble -> next
      }
  }
  def parseArrayItem[I](n: Int, next: Parse[I]): Parse[I] = { implicit path => s =>
    def recurse(left: Int): Parse[I] = { implicit path => stream =>
      if (left == 0) {
        next(path)(skipWS(stream))
      } else {
        val skip1        = skipOne(Comma)(path)(stream)
        val skippedComma = skipComma(path)(skipWS(skip1))
        recurse(left - 1)(path)(skippedComma)
      }
    }

    skipWS(s).take1.flatMap {
      case ('[', next) => recurse(n)(path \ n)(next)
      case (u, _)      => ME.raiseError(ParseFailure("[", u.toString, path \ n))
    }
  }
  def parseObj[I](k: String, nextOp: Parse[I]): Parse[I] = { implicit path => stream =>
    def skipUntilKey(implicit path: Vector[PPath]): Pipe = { s =>
      parseObjKey(s).take1.flatMap[Char] {
        case ((key, nextS), _) if key == k => nextS
        case ((_, nextS), _) =>
          val skippedOne = skipOne(Comma)(path)(nextS)
          val dropComma  = skipWS(skippedOne).drop1

          skipUntilKey(path)(dropComma)
      }
    }

    nextOp(path \ k) {
      skipWS(stream).take1.flatMap {
        case ('{', next) => skipUntilKey(path \ k)(next)
        case (c, _)      => ME.raiseError(ParseFailure("{", c.toString, path))
      }
    }

  }

  // todo: handle unicode ??? (maybe just dont support it)
  /**
    * Method to accumulate json string
    * Expect input to NOT starts with double quote, expects String to end with
    * double quote
    *
    * Returns a Source of Vector[Char] and Source[Char]
    *   Vector[Char] represents the js string without double quotes, can be empty
    *   Source[Char] represent subsequent stream of char after consuming string
    *
    * eg. Input = I am pretty", next...
    *     Output = (I am pretty -> Source(, next...))
    * @param i
    * @param path
    * @return
    */
  private def accJsString(i: Source[Char])(
      implicit path: Vector[PPath]): Source[(Vector[Char], Source[Char])] = {

    // lastCharIsSpecial - we need to know if last
    // char is special or not, so that we can
    // know if the last `\\` starts a new escape
    // sequence, eg. acc = "\\\\", then
    // we should not treat the next char as part of
    // escape sequence
    def recurse(s: Source[Char])(
        acc: Vector[Char],
        lastCharIsSpecial: Boolean): Source[(Vector[Char], Source[Char])] = {
      val wasEscaped = !lastCharIsSpecial && acc.lastOption.contains('\\')

      s.take1.flatMap {
        case ('"', next) if wasEscaped =>
          recurse(next)(acc.dropRight(1) :+ '"', true)
        case ('/', next) if wasEscaped =>
          recurse(next)(acc.dropRight(1) :+ '/', true)
        case ('b', next) if wasEscaped => recurse(next)(acc :+ 'b', true)
        case ('f', next) if wasEscaped => recurse(next)(acc :+ 'f', true)
        case ('n', next) if wasEscaped => recurse(next)(acc :+ 'n', true)
        case ('r', next) if wasEscaped => recurse(next)(acc :+ 'r', true)
        case ('t', next) if wasEscaped => recurse(next)(acc :+ 't', true)
        case ('\\', next) if wasEscaped =>
          recurse(next)(acc.dropRight(1) :+ '\\', true)
        case (oops, _) if wasEscaped =>
          ME.raiseError(ParseFailure(s"Illegal escape sequence \\$oops", path))
        case ('"', next) => ME.pure(acc -> next)
        case (c, next)   => recurse(next)(acc :+ c, false)
      }
    }

    recurse(i)(Vector.empty, lastCharIsSpecial = false)
  }

  private def skipWS(s: CharSource): CharSource = {
    s.take1.flatMap[Char] {
      case (whitespace(_), next) => skipWS(next)
      case (c, next)             => Cons.cons(next, c)
    }
  }

  private def skipStr(implicit path: Vector[PPath]): Pipe = { s =>
    parseString(path)(s).flatMap[Char](_._2)
  }
  private def skipBool(implicit path: Vector[PPath]): Pipe =
    s => parseBoolean(path)(s).flatMap[Char](_._2)
  private def skipComma(implicit path: Vector[PPath]): Pipe = { s =>
    s.take1.flatMap {
      case (',', next) => next
      case (u, _)      => ME.raiseError(ParseFailure(",", u.toString, path))
    }
  }
  private def skipNum(expectedTerminator: ExpectedTerminator)(
      implicit path: Vector[PPath]): Pipe = { s =>
    parseNumber(expectedTerminator)(path)(s).flatMap[Char](_._2)
  }

  // Does not skip intermediate terminator, eg. `,`
  private def skipOne(term: ExpectedTerminator)(implicit path: Vector[PPath]): Pipe = { stream =>
    skipWS(stream).peek1.flatMap {
      case ('"', next)      => skipStr(path)(next)
      case ('t', next)      => skipBool(path)(next)
      case ('f', next)      => skipBool(path)(next)
      case (digit(_), next) => skipNum(term)(path)(next)
      case (sign(_), next)  => skipNum(term)(path)(next)
      case ('[', next)      => skipArr(path)(next)
      case ('{', next)      => skipObject(path)(next)
      case (unexp, _) =>
        ME.raiseError(ParseFailure("One of(t, f, [, {)", unexp.toString, path))
    }
  }

  private def skipArr(implicit path: Vector[PPath]): Pipe = { stream =>
    stream.take1.flatMap {
      // expect arr to starts with [
      case ('[', next) =>
        next.peek1.flatMap[Char] {
          // check if it's empty array
          case (']', next) => next.drop1
          case (_, next)   => takeTilArrayEnd(path)(next)
        }
      case (ue, _) => ME.raiseError(ParseFailure("[", ue.toString, path))
    }
  }

  private def takeTilArrayEnd(implicit path: Vector[PPath]): Pipe = { s =>
    val skipped = skipOne(OneOf(Bracket, Comma))(path)(s)

    skipWS(skipped).take1.flatMap {
      case (']', next) => next
      case (',', next) => takeTilArrayEnd(path)(next)
      case (unexp, _)  => ME.raiseError(ParseFailure(",", unexp.toString, path))
    }
  }

  private def parseObjKey(stream: CharSource)(
      implicit path: Vector[PPath]): Source[(String, CharSource)] = {

    skipWS(stream).take1.flatMap {
      case ('"', nextStream) =>
        val keyStr = accJsString(nextStream)

        keyStr.flatMap {
          case (key, afterKey) =>
            skipWS(afterKey).take1.flatMap {
              case (':', next) => ME.pure(key.mkString -> next)
              case (o, _) =>
                ME.raiseError(ParseFailure(s": for key ${key.mkString} finding", o.toString, path))
            }
        }

      case (uexp, _) =>
        ME.raiseError(ParseFailure("\" to start a key", uexp.toString, path))
    }
  }

  private def skipKVPair(implicit path: Vector[PPath]): Pipe = { s =>
    parseObjKey(s).flatMap {
      case (_, next) =>
        val skippedOne = skipOne(OneOf(Comma, CurlyBrace))(path)(next)

        skipWS(skippedOne).peek1.flatMap {
          case (',', next) => skipKVPair(path)(next.drop1)
          case ('}', next) => next
          case (uexp, _) =>
            ME.raiseError(ParseFailure(", or }", uexp.toString, path))
        }
    }
  }

  private def skipObject(implicit path: Vector[PPath]): Pipe = { s =>
    s.take1.flatMap {
      case ('{', next) =>
        skipKVPair(path)(next).take1.flatMap[Char] {
          case ('}', next) => next
          case (uexp, _) =>
            ME.raiseError(ParseFailure("}", uexp.toString, path))
        }
      case (uexp, _) =>
        ME.raiseError[Char](ParseFailure("{", uexp.toString, path))
    }
  }
  private def parseSign(s: CharSource): Source[(Option[Char], CharSource)] = {
    s.take1.flatMap {
      case (sign(sChar), next) => ME.pure(Some(sChar) -> next)
      case _                   => ME.pure(None        -> s)
    }
  }

  implicit class SourceOps[A](s: Source[(Option[A], CharSource)]) {
    def flatFold[B](f: (A, CharSource) => Source[B])(orElse: Source[B]): Source[B] = {
      s.flatMap {
        case (Some(a), next) => f(a, next)
        case (None, _)       => orElse
      }
    }
  }

  private case class Part1(part1: Vector[Char], sep: Option[Char], cont: CharSource)
  private case class Part2(part2: Vector[Char], sep: Option[Char], cont: CharSource)

  private def consumeTillTermination[A](s: CharSource)(
      term: ExpectedTerminator,
      f: CharSource => Source[A])(implicit p: Vector[PPath]): Source[A] = {
    skipWS(s).take1.flatMap {
      case (t, _) if term.matchChar(t) => f(s)
      case (uexp, _) =>
        ME.raiseError(ParseFailure(term.toString, uexp.toString, p))
    }
  }

  private def parseNum1(s: CharSource)(term: ExpectedTerminator)(
      implicit p: Vector[PPath]): Source[Part1] = {
    def recurse(acc: Vector[Char], s: CharSource): Source[Part1] = {
      s.take1Opt.flatFold {
        case (digit(d), next)            => recurse(acc :+ d, next)
        case (t, _) if term.matchChar(t) => ME.pure(Part1(acc, None, s))
        case ('.', next)                 => ME.pure(Part1(acc, Some('.'), next))
        case ('E', next)                 => ME.pure(Part1(acc, Some('e'), next))
        case ('e', next)                 => ME.pure(Part1(acc, Some('e'), next))
        case (whitespace(_), next) =>
          consumeTillTermination(next)(term, n => ME.pure(Part1(acc, None, n)))
        case (u, _) =>
          ME.raiseError[Part1](ParseFailure(s"Digit or $term", u.toString, p))
      } {
        if (term == End && acc.nonEmpty) {
          ME.pure(Part1(acc, None, Monoid.empty))
        } else {
          ME.raiseError(ParseFailure.termination)
        }
      }
    }
    parseSign(s).flatMap {
      case (maybe, next) => recurse(maybe.toVector, next)
    }
  }

  private def parseNum2(s: CharSource, p1Sep: Char)(term: ExpectedTerminator)(
      implicit p: Vector[PPath]): Source[Part2] = {
    def recurse(acc: Vector[Char], s: CharSource): Source[Part2] = {
      s.take1Opt.flatFold {
        case (digit(d), next)            => recurse(acc :+ d, next)
        case (t, _) if term.matchChar(t) => ME.pure(Part2(acc, None, s))
        case (exponent(e), next) if p1Sep == '.' =>
          ME.pure(Part2(acc, Some(e), next))
        case (whitespace(_), next) =>
          consumeTillTermination(next)(term, n => ME.pure(Part2(acc, None, n)))
        case (u, _) =>
          ME.raiseError[Part2](ParseFailure(s"Digit or $term", u.toString, p))
      } {
        if (term == End && acc.nonEmpty) {
          ME.pure(Part2(acc, None, Monoid.empty))
        } else {
          ME.raiseError(ParseFailure.termination)
        }
      }
    }
    recurse(Vector.empty, s)
  }

  private def parseNum3(s: CharSource)(term: ExpectedTerminator)(
      implicit path: Vector[PPath]): Source[(Vector[Char], CharSource)] = {
    def recurse(acc: Vector[Char], s: CharSource): Source[(Vector[Char], CharSource)] = {
      s.take1Opt.flatFold {
        case (digit(d), next)            => recurse(acc :+ d, next)
        case (t, _) if term.matchChar(t) => ME.pure(acc -> s)
        case (whitespace(_), next) =>
          consumeTillTermination(next)(term, n => ME.pure(acc -> n))
        case (u, _) =>
          ME.raiseError[(Vector[Char], CharSource)](
            ParseFailure(s"Digit or $term", u.toString, path))
      } {
        if (term == End && acc.nonEmpty) {
          ME.pure(acc -> s)
        } else {
          ME.raiseError(ParseFailure.termination)
        }
      }
    }
    parseSign(s).flatMap {
      case (maybe, next) => recurse(maybe.toVector, next)
    }
  }

  implicit val ParseApp: Applicative[Parse] = new Applicative[Parse] {
    override def pure[A](x: A): Parse[A] = { path => src =>
      ME.pure(x -> src)
    }
    override def ap[A, B](ff: Parse[A => B])(fa: Parse[A]): Parse[B] = { path => src =>
      for {
        pair1     <- ff(path)(src)
        (a2b, _)  = pair1
        pair2     <- fa(path)(src)
        (a, rest) = pair2
      } yield {
        a2b(a) -> rest
      }
    }
  }

  private def parseProduct[I](all: FreeApplicative[ParseOps[Parse, ?], I]): Parse[I] = {
    all.foldMap(parsing)
  }

  private def parseOneOf[I](oneOf: NonEmptyList[Parse[I]]): Parse[I] = { path => src =>
    val h = oneOf.head
    oneOf.tail.toList match {
      case Nil => h(path)(src)
      case nonEmpty =>
        h(path)(src).recoverWith {
          case _ => parseOneOf(NonEmptyList.fromListUnsafe(nonEmpty))(path)(src)
        }
    }

  }

  private def parseOptional[I](parse: Parse[I]): Parse[Option[I]] = { path => src =>
    parse(path)(src)
      .map[(Option[I], CharSource)] {
        case (i, next) =>
          // WARNING: runtime flattening of None
          // We dont flatten nested Some (eg. Some(Some(x)) => Some(x))
          // because that would break the type
          // ie. None <: Option[Option[Option[?]]] is generally true
          // but Some(x) <: Option[Option[Option[?]]] is not
          i match {
            case None => None    -> next
            case _    => Some(i) -> next
          }
      }
      .recover {
        case _ => None -> src
      }
  }

  val parsing: ParseOps[Parse, ?] ~> Parse = new (ParseOps[Parse, ?] ~> Parse) {
    override def apply[A](fa: ParseOps[Parse, A]): Parse[A] = {
      val parseA = fa match {
        case GetString                   => parseString
        case GetBool                     => parseBoolean
        case GetNum(t)                   => parseNumber(t)
        case getN: GetN[Parse, i]        => parseArrayItem(getN.n, getN.next)
        case getK: GetKey[Parse, i]      => parseObj(getK.key, getK.next)
        case GetProduct(all)             => parseProduct(all)
        case GetSum(oneOf)               => parseOneOf(oneOf)
        case getOpt: GetOpt[Parse, i]    => parseOptional(getOpt.next)
        case mapped: Mapped[Parse, h, A] => mapped.fi.map(mapped.fn)
      }
      // is this harmful??
      parseA.asInstanceOf[Parse[A]]
    }
  }
}
