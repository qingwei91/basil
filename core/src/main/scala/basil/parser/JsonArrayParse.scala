package basil.parser

import basil.data._
import basil.typeclass.Lazy
import cats.data.NonEmptyMap
import cats.implicits._
import cats.{Applicative, MonadError, ~>}

import scala.util.Try

/**
  * TODO: model Input as `(Array[Char], Int)`
  * this will avoid having to inject `Array[Char]` when using this
  *
  * We only need 3 abilities from the input
  * a) get by index
  * b) slice
  * c) isDefinedAtIndex
  */
abstract class JsonArrayParse[F[_]](fullSource: Array[Char])(
    implicit
    ME: MonadError[F, ParseFailure],
) extends JsonParse[Int, Lambda[A => F[(A, Int)]]] {

  val discriminatorField: String = "type"

  // latest index
  type CharSource = Int

  type Pipe = CharSource => F[CharSource]

  private def getChar(i: Int)(implicit path: Vector[PPath]): F[Char] = {
    ME.fromEither {
      try {
        Right(fullSource(i))
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          Left(ParseFailure.termination)
      }
    }
  }

  private def getCharOpt(i: Int): F[Option[Char]] = {
    Try {
      Option(fullSource(i))
    }.fold(_ => None, identity).pure[F]
  }

  val parseString: Parse[String] = { implicit path => src =>
    val latestI = skipWS(src)

    for {
      firstC <- getChar(latestI)
      result <- firstC match {
                 case '"' => accJsString(latestI + 1)
                 case other =>
                   ME.raiseError[(String, Int)](
                     ParseFailure(s"""String should starts with ", but found $other""", path))
               }
    } yield result
  }

  implicit class PointerOps(i: CharSource) {
    def isFollowedBy(str: String): (Boolean, Int) = {
      val slice = fullSource.slice(i, i + str.length)
      slice.sameElements(str.toCharArray) -> (i + str.length)
    }
  }

  val parseBoolean: Parse[Boolean] = { implicit path => src =>
    val withoutWS      = skipWS(src)
    val (isTrue, next) = withoutWS.isFollowedBy("true")

    for {
      result <- if (isTrue) {
                 ME.pure(true -> next)
               } else {
                 val (isFalse, next) = withoutWS.isFollowedBy("false")
                 for {
                   inner <- if (isFalse) {
                             ME.pure(false -> next)
                           } else {
                             ME.raiseError[(Boolean, Int)](
                               ParseFailure("Expecting either `true` or `false`", path))
                           }
                 } yield inner
               }
    } yield result
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
        for {
          skip1        <- skipOne(Comma)(path)(stream)
          skippedComma <- skipComma(path)(skipWS(skip1))
          result       <- recurse(left - 1)(path)(skippedComma)
        } yield {
          result
        }
      }
    }
    val skipped = skipWS(s)

    getChar(skipped).flatMap {
      case '[' => recurse(n)(path \ n)(skipped + 1)
      case u   => ME.raiseError(ParseFailure("[", u.toString, path \ n))
    }
  }
  def parseObj[I](k: String, nextOp: Parse[I]): Parse[I] = { implicit path => stream =>
    def skipUntilKey(implicit path: Vector[PPath]): Pipe = { s =>
      parseObjKey(s).flatMap {
        case (key, nextS) if key == k => nextS.pure[F]
        case (_, nextS) =>
          for {
            skippedOne <- skipOne(Comma)(path)(nextS)
            dropComma  = skipWS(skippedOne) + 1
            untilKey   <- skipUntilKey(path)(dropComma)
          } yield {
            untilKey
          }
      }
    }

    val skipped = skipWS(stream)
    val afterKeyF = getChar(skipped).flatMap {
      case '{' => skipUntilKey(path \ k)(skipped + 1)
      case c   => ME.raiseError[CharSource](ParseFailure("{", c.toString, path))
    }

    for {
      afterKey <- afterKeyF
      result   <- nextOp(path \ k)(afterKey)
    } yield {
      result
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
  private def accJsString(i: Int)(implicit path: Vector[PPath]): F[(String, Int)] = {

    // lastCharIsSpecial - we need to know if last
    // char is special or not, so that we can
    // know if the last `\\` starts a new escape
    // sequence, eg. acc = "\\\\", then
    // we should not treat the next char as part of
    // escape sequence

    type Input  = (Int, Vector[Char], Boolean)
    type Output = (Vector[Char], Int)

    def recurse(a: Input): F[Either[Input, Output]] = {
      val (s, acc, lastCharIsSpecial) = a

      val wasEscaped = !lastCharIsSpecial && acc.lastOption.contains('\\')

      val next = s + 1
      getChar(s).flatMap {
        case '"' if wasEscaped =>
          (next, acc.dropRight(1) :+ '"', true).asLeft[Output].pure[F]
        case '/' if wasEscaped =>
          (next, acc.dropRight(1) :+ '/', true).asLeft[Output].pure[F]
        case 'b' if wasEscaped =>
          (next, acc :+ 'b', true).asLeft[Output].pure[F]
        case 'f' if wasEscaped =>
          (next, acc :+ 'f', true).asLeft[Output].pure[F]
        case 'n' if wasEscaped =>
          (next, acc :+ 'n', true).asLeft[Output].pure[F]
        case 'r' if wasEscaped =>
          (next, acc :+ 'r', true).asLeft[Output].pure[F]
        case 't' if wasEscaped =>
          (next, acc :+ 't', true).asLeft[Output].pure[F]
        case '\\' if wasEscaped =>
          (next, acc.dropRight(1) :+ '\\', true).asLeft[Output].pure[F]
        case oops if wasEscaped =>
          ME.raiseError(ParseFailure(s"Illegal escape sequence \\$oops", path))
        case '"' => ME.pure((acc -> next).asRight[Input])
        case c   => (next, acc :+ c, false).asLeft[Output].pure[F]
      }
    }

    ME.tailRecM((i, Vector.empty[Char], false))(recurse)
      .map {
        case (acc, next) => acc.mkString -> next
      }
  }

  private def skipWS(s: CharSource): CharSource = {
    if (fullSource.isDefinedAt(s)) {
      fullSource(s) match {
        case whitespace(_) => skipWS(s + 1)
        case _             => s
      }
    } else {
      s
    }
  }

  private def skipStr(implicit path: Vector[PPath]): Pipe = { s =>
    parseString(path)(s).map(_._2)
  }
  private def skipBool(implicit path: Vector[PPath]): Pipe =
    s => parseBoolean(path)(s).map(_._2)

  private def skipComma(implicit path: Vector[PPath]): Pipe = { s =>
    getChar(s).flatMap {
      case ',' => (s + 1).pure[F]
      case u   => ME.raiseError(ParseFailure(",", u.toString, path))
    }
  }
  private def skipNum(expectedTerminator: ExpectedTerminator)(
      implicit path: Vector[PPath]): Pipe = { s =>
    parseNumber(expectedTerminator)(path)(s).map(_._2)
  }

  // Does not skip intermediate terminator, eg. `,`
  private def skipOne(term: ExpectedTerminator)(implicit path: Vector[PPath]): Pipe = { stream =>
    val noWS = skipWS(stream)
    getChar(noWS).flatMap {
      case '"'      => skipStr(path)(noWS)
      case 't'      => skipBool(path)(noWS)
      case 'f'      => skipBool(path)(noWS)
      case digit(_) => skipNum(term)(path)(noWS)
      case sign(_)  => skipNum(term)(path)(noWS)
      case '['      => skipArr(path)(noWS)
      case '{'      => skipObject(path)(noWS)
      case unexp =>
        ME.raiseError(ParseFailure("One of(t, f, [, {)", unexp.toString, path))
    }
  }

  private def skipArr(implicit path: Vector[PPath]): Pipe = { stream =>
    val next = stream + 1
    getChar(stream).flatMap {
      // expect arr to starts with [
      case '[' =>
        getChar(next).flatMap {
          case ']' => next.pure[F]
          case _   => takeTilArrayEnd(path)(next)
        }
      case ue => ME.raiseError(ParseFailure("[", ue.toString, path))
    }
  }

  private def takeTilArrayEnd(implicit path: Vector[PPath]): Pipe = { s =>
    for {
      skippedSep <- skipOne(OneOf(Bracket, Comma))(path)(s)
      skippedWs  = skipWS(skippedSep)
      result <- getChar(skippedWs).flatMap {
                 case ']'   => (skippedWs + 1).pure[F]
                 case ','   => takeTilArrayEnd(path)(skippedWs + 1)
                 case unexp => ME.raiseError[CharSource](ParseFailure(",", unexp.toString, path))
               }
    } yield result

  }

  private def parseObjKey(stream: CharSource)(
      implicit path: Vector[PPath]): F[(String, CharSource)] = {
    val skipped = skipWS(stream)

    getChar(skipped).flatMap {
      case '"' =>
        val keyStr = accJsString(skipped + 1)

        keyStr.flatMap {
          case (key, afterKey) =>
            val skipAfterKey = skipWS(afterKey)
            getChar(skipAfterKey).flatMap {
              case ':' => (key, skipAfterKey + 1).pure[F]
              case o =>
                ME.raiseError[(String, Int)](
                  ParseFailure(s": for key ${key.mkString} finding", o.toString, path))
            }
        }

      case uexp =>
        ME.raiseError(ParseFailure("\" to start a key", uexp.toString, path))
    }
  }

  private def skipKVPairs(implicit path: Vector[PPath]): Pipe = { s =>
    parseObjKey(s).flatMap {
      case (_, next) =>
        for {
          skippedOne <- skipOne(OneOf(Comma, CurlyBrace))(path)(next)
          skippedWS  = skipWS(skippedOne)
          result <- getChar(skippedWS).flatMap {
                     case ',' => skipKVPairs(path)(skippedWS)
                     case '}' => (skippedWS + 1).pure[F]
                     case uexp =>
                       ME.raiseError[CharSource](ParseFailure(", or }", uexp.toString, path))
                   }
        } yield result

    }
  }

  private def skipObject(implicit path: Vector[PPath]): Pipe = { s =>
    getChar(s).flatMap {
      case '{' =>
        for {
          skipKVs <- skipKVPairs(path)(s + 1)
          result <- getChar(skipKVs).flatMap {
                     case '}'  => (skipKVs + 1).pure[F]
                     case uexp => ME.raiseError[CharSource](ParseFailure("}", uexp.toString, path))
                   }
        } yield {
          result
        }
      case uexp =>
        ME.raiseError[CharSource](ParseFailure("{", uexp.toString, path))
    }
  }
  private def parseSign(s: CharSource)(
      implicit path: Vector[PPath]): F[(Option[Char], CharSource)] = {
    getChar(s).map {
      case sign(sChar) => Some(sChar) -> (s + 1)
      case _           => None        -> s
    }
  }

  implicit class SourceOps[A](s: F[Option[A]]) {
    def flatFold[B](f: A => F[B])(orElse: F[B]): F[B] = {
      s.flatMap {
        case Some(a) => f(a)
        case None    => orElse
      }
    }
  }

  private case class Part1(part1: Vector[Char], sep: Option[Char], cont: CharSource)
  private case class Part2(part2: Vector[Char], sep: Option[Char], cont: CharSource)

  private def consumeTillTermination[A](s: CharSource)(
      term: ExpectedTerminator,
      f: CharSource => F[A])(implicit p: Vector[PPath]): F[A] = {
    getChar(s).flatMap {
      case c if term.matchChar(c) => f(s)
      case uexp                   => ME.raiseError[A](ParseFailure(term.toString, uexp.toString, p))
    }
  }

  private def parseNum1(s: CharSource)(term: ExpectedTerminator)(
      implicit p: Vector[PPath]): F[Part1] = {
    def recurse(acc: Vector[Char], s: CharSource): F[Part1] = {
      val next = s + 1
      getCharOpt(s).flatFold {
        case digit(d)               => recurse(acc :+ d, next)
        case t if term.matchChar(t) => ME.pure(Part1(acc, None, s))
        case '.'                    => ME.pure(Part1(acc, Some('.'), next))
        case 'E'                    => ME.pure(Part1(acc, Some('e'), next))
        case 'e'                    => ME.pure(Part1(acc, Some('e'), next))
        case whitespace(_) =>
          consumeTillTermination(next)(term, n => ME.pure(Part1(acc, None, n)))
        case u =>
          ME.raiseError[Part1](ParseFailure(s"Digit or $term", u.toString, p))
      } {
        if (term == End && acc.nonEmpty) {
          ME.pure(Part1(acc, None, s))
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
      implicit p: Vector[PPath]): F[Part2] = {
    def recurse(acc: Vector[Char], s: CharSource): F[Part2] = {
      val next = s + 1
      getCharOpt(s).flatFold {
        case digit(d)                    => recurse(acc :+ d, next)
        case t if term.matchChar(t)      => ME.pure(Part2(acc, None, s))
        case exponent(e) if p1Sep == '.' => ME.pure(Part2(acc, Some(e), next))
        case whitespace(_) =>
          consumeTillTermination(next)(term, n => ME.pure(Part2(acc, None, n)))
        case u =>
          ME.raiseError[Part2](ParseFailure(s"Digit or $term", u.toString, p))
      } {
        if (term == End && acc.nonEmpty) {
          ME.pure(Part2(acc, None, s))
        } else {
          ME.raiseError(ParseFailure.termination)
        }
      }
    }
    recurse(Vector.empty, s)
  }

  private def parseNum3(s: CharSource)(term: ExpectedTerminator)(
      implicit path: Vector[PPath]): F[(Vector[Char], CharSource)] = {
    def recurse(acc: Vector[Char], s: CharSource): F[(Vector[Char], CharSource)] = {
      val next = s + 1
      getCharOpt(s).flatFold {
        case digit(d)               => recurse(acc :+ d, next)
        case t if term.matchChar(t) => ME.pure(acc -> s)
        case whitespace(_) =>
          consumeTillTermination(next)(term, n => ME.pure(acc -> n))
        case u =>
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
        pair1           <- ff(path)(src)
        (fn, _)         = pair1
        pair2           <- fa(path)(src)
        (a, restSource) = pair2
      } yield {
        fn(a) -> restSource
      }
    }
  }

  def parseOneOf[I](oneOf: NonEmptyMap[String, Lazy[Parse[I]]]): Parse[I] = { path => src =>
    parseObj(discriminatorField, parseString)(path)(src).flatMap {
      case (key, _) =>
        oneOf(key) match {
          case Some(parseFn) => parseFn.value(path)(src)
          case None =>
            ME.raiseError[(I, CharSource)](ParseFailure(s"Cannot parse object with type=$key"))
        }
    }
  }

  def parseOptional[I](parse: Parse[I]): Parse[Option[I]] = { path => src =>
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

  val parsing: ParseOps[Parse, ?] ~> Parse = parsingM
}
