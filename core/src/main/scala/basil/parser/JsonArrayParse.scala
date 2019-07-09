package basil.parser

import basil.data._
import basil.typeclass.Lazy
import cats.data.NonEmptyMap
import cats.implicits._
import cats.{Applicative, MonadError, ~>}

import JsonArrayParse._

/**
  * TODO: model Input as `(Array[Char], Int)`
  * this will avoid having to inject `Array[Char]` when using this
  *
  * We only need 3 abilities from the input
  * a) get by index
  * b) slice
  * c) isDefinedAtIndex
  */
abstract class JsonArrayParse[F[_]](
    implicit ME: MonadError[F, ParseFailure],
) extends JsonParse[Input, Output[F, ?]] {

  private val discriminatorField: String = "type"

  type Pipe = Input => Input

  private def getChar(input: Input): Char = {
    val (fullSource, i) = input
    fullSource(i)
  }

  private def getCharOpt(input: Input): Option[Char] = {
    val (fullSource, i) = input
    try {
      Some(fullSource(i))
    } catch {
      case _: ArrayIndexOutOfBoundsException => None
    }
  }

  private def wrap[A](a: => A)(implicit path: Vector[PPath]): F[A] = ME.fromEither {
    try {
      Right(a)
    } catch {
      case _: ArrayIndexOutOfBoundsException => Left(ParseFailure.termination)
      case e: ParseFailure                   => Left(e)
    }
  }

  private def unsafeParseString(src: Input)(implicit path: Vector[PPath]): (String, Input) = {
    val latestI = skipWS(src)
    val firstC  = getChar(latestI)

    firstC match {
      case '"' =>
        val (str, next) = accJsString(latestI.next)
        str -> src.pointTo(next)
      case other =>
        throw ParseFailure(s"""String should starts with ", but found $other""", path)
    }
  }

  val parseString: Parse[String] = { implicit path => src =>
    wrap {
      unsafeParseString(src)
    }
  }

  private def unsafeParseBool(src: Input)(implicit path: Vector[PPath]): (Boolean, Input) = {
    val withoutWS      = skipWS(src)
    val (isTrue, next) = withoutWS.isFollowedBy("true")

    if (isTrue) {
      true -> src.pointTo(next)
    } else {
      val (isFalse, next) = withoutWS.isFollowedBy("false")
      if (isFalse) {
        false -> src.pointTo(next)
      } else {
        throw ParseFailure("Expecting either `true` or `false`", path)
      }
    }
  }

  val parseBoolean: Parse[Boolean] = { implicit path => src =>
    wrap {
      unsafeParseBool(src)
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
  private def unsafeParseNum(terminator: ExpectedTerminator, input: Input)(
      implicit path: Vector[PPath]): (Double, Input) = {
    parseNum1(skipWS(input))(terminator) match {
      case Part1(p1, Some(sep1), next) =>
        parseNum2(next, sep1)(terminator) match {
          case Part2(p2, Some(sep2), next) =>
            parseNum3(next)(terminator) match {
              case (p3, next) =>
                val full = p1
                  .append(sep1)
                  .append(p2.append(sep2))
                  .append(p3)

                full.mkString.toDouble -> next
            }
          case Part2(p2, None, next) =>
            val full = (p1 :+ sep1) ++ p2
            full.mkString.toDouble -> next
        }
      case Part1(p1, None, next) => p1.mkString.toDouble -> next
    }
  }

  def parseNumber(terminator: ExpectedTerminator): Parse[Double] = { implicit path => s =>
    wrap {
      unsafeParseNum(terminator, s)
    }
  }

  def parseArrayItem[I](n: Int, next: Parse[I]): Parse[I] = { implicit path => s =>
    def recurse(left: Int, path: Vector[PPath], stream: Input): Output[F, I] = {
      if (left == 0) {
        next(path)(skipWS(stream))
      } else {

        val skip1        = skipOne(Comma)(path)(stream)
        val skippedComma = skipComma(path)(skipWS(skip1))
        recurse(left - 1, path, skippedComma)
      }
    }

    wrap {
      val skipped = skipWS(s)
      getChar(skipped) match {
        case '[' => recurse(n, path \ n, skipped.next)
        case u   => ME.raiseError[(I, Input)](ParseFailure("[", u.toString, path \ n))
      }
    }.flatten
  }

  def parseObj[I](k: String, nextOp: Parse[I]): Parse[I] = { implicit path => stream =>
    def skipUntilKey(s: Input)(implicit path: Vector[PPath]): Input = {
      parseObjKey(s) match {
        case (key, nextS) if key == k => nextS
        case (_, nextS) =>
          val skippedOne = skipOne(Comma)(path)(nextS)
          val dropComma  = skipWS(skippedOne).next
          skipUntilKey(dropComma)
      }
    }

    wrap {
      val skipped = skipWS(stream)
      getChar(skipped) match {
        case '{' => skipUntilKey(skipped.next)(path \ k)
        case c   => throw ParseFailure("{", c.toString, path)
      }
    }.flatMap { afterKey =>
      nextOp(path \ k)(afterKey)
    }

  }

  // todo: handle unicode ??? (maybe just dont support it)
  private def accJsString(input: Input)(implicit path: Vector[PPath]): (String, Int) = {

    // lastCharIsSpecial - we need to know if last
    // char is special or not, so that we can
    // know if the last `\\` starts a new escape
    // sequence, eg. acc = "\\\\", then
    // we should not treat the next char as part of
    // escape sequence

    // TODO: consider optimize acc to just be a pair of pointers
    // then we can avoid allocation
    def recurse(curPointer: Int, acc: StringBuilder, lastCharIsSpecial: Boolean): (String, Int) = {

      val wasEscaped = !lastCharIsSpecial && acc.lastOption.contains('\\')

      val next = curPointer + 1

      getChar((input._1, curPointer)) match {
        case '"' if wasEscaped =>
          val updated = acc.deleteCharAt(acc.size - 1).append('"')
          recurse(next, updated, true)
        case '/' if wasEscaped =>
          val updated = acc.deleteCharAt(acc.size - 1).append('/')
          recurse(next, updated, true)
        case 'b' if wasEscaped =>
          recurse(next, acc.append('b'), true)
        case 'f' if wasEscaped =>
          recurse(next, acc.append('f'), true)
        case 'n' if wasEscaped =>
          recurse(next, acc.append('n'), true)
        case 'r' if wasEscaped =>
          recurse(next, acc.append('r'), true)
        case 't' if wasEscaped =>
          recurse(next, acc.append('t'), true)
        case '\\' if wasEscaped =>
          val updated = acc.deleteCharAt(acc.size - 1).append('\\')
          recurse(next, updated, true)
        case oops if wasEscaped =>
          throw ParseFailure(s"Illegal escape sequence \\$oops", path)
        case '"' => (acc.mkString, next)
        case c   => recurse(next, acc.append(c), false)
      }
    }

    recurse(input._2, StringBuilder.newBuilder, lastCharIsSpecial = false)
  }

  private def skipWS(s: Input): Input = {
    val (fullSource, i) = s
    if (fullSource.isDefinedAt(i)) {
      fullSource(i) match {
        case whitespace(_) => skipWS(s.next)
        case _             => s
      }
    } else {
      s
    }
  }

  private def skipStr(implicit path: Vector[PPath]): Pipe = { s =>
    unsafeParseString(s)._2
  }
  private def skipBool(implicit path: Vector[PPath]): Pipe =
    s => unsafeParseBool(s)._2

  private def skipComma(implicit path: Vector[PPath]): Pipe = { s =>
    getChar(s) match {
      case ',' => s.next
      case u   => throw ParseFailure(",", u.toString, path)
    }
  }
  private def skipNum(expectedTerminator: ExpectedTerminator)(
      implicit path: Vector[PPath]): Pipe = { s =>
    unsafeParseNum(expectedTerminator, s)._2
  }

  // Does not skip intermediate terminator, eg. `,`
  private def skipOne(term: ExpectedTerminator)(implicit path: Vector[PPath]): Pipe = { stream =>
    val noWS = skipWS(stream)
    getChar(noWS) match {
      case '"'      => skipStr(path)(noWS)
      case 't'      => skipBool(path)(noWS)
      case 'f'      => skipBool(path)(noWS)
      case digit(_) => skipNum(term)(path)(noWS)
      case sign(_)  => skipNum(term)(path)(noWS)
      case '['      => skipArr(path)(noWS)
      case '{'      => skipObject(path)(noWS)
      case unexp =>
        throw ParseFailure("One of(t, f, [, {)", unexp.toString, path)
    }
  }

  private def skipArr(implicit path: Vector[PPath]): Pipe = { stream =>
    val next = stream.next
    getChar(stream) match {
      // expect arr to starts with [
      case '[' =>
        getChar(next) match {
          case ']' => next
          case _   => takeTilArrayEnd(next)
        }
      case ue => throw ParseFailure("[", ue.toString, path)
    }
  }

  private def takeTilArrayEnd(s: Input)(implicit path: Vector[PPath]): Input = {
    val skippedSep = skipOne(OneOf(Bracket, Comma))(path)(s)
    val skippedWs  = skipWS(skippedSep)

    getChar(skippedWs) match {
      case ']'   => skippedWs.next
      case ','   => takeTilArrayEnd(skippedWs.next)
      case unexp => throw ParseFailure(",", unexp.toString, path)
    }

  }

  private def parseObjKey(input: Input)(implicit path: Vector[PPath]): (String, Input) = {
    val skipped = skipWS(input)

    getChar(skipped) match {
      case '"' =>
        val keyStr = accJsString(skipped.next)

        keyStr match {
          case (key, afterKey) =>
            val skipAfterKey = skipWS(input._1 -> afterKey)
            getChar(skipAfterKey) match {
              case ':' => (key, skipAfterKey.next)
              case o =>
                throw ParseFailure(s": for key ${key.mkString} finding", o.toString, path)
            }
        }

      case uexp => throw ParseFailure("\" to start a key", uexp.toString, path)
    }
  }

  private def skipKVPairs(s: Input)(implicit path: Vector[PPath]): Input = {
    parseObjKey(s) match {
      case (_, next) =>
        val skippedOne = skipOne(OneOf(Comma, CurlyBrace))(path)(next)
        val skippedWS  = skipWS(skippedOne)
        getChar(skippedWS) match {
          case ','  => skipKVPairs(skippedWS.next)
          case '}'  => skippedWS
          case uexp => throw ParseFailure(", or }", uexp.toString, path)
        }

    }
  }

  private def skipObject(implicit path: Vector[PPath]): Pipe = { s =>
    getChar(s) match {
      case '{' =>
        val skipKVs = skipKVPairs(s.next)
        getChar(skipKVs) match {
          case '}'  => skipKVs.next
          case uexp => throw ParseFailure("}", uexp.toString, path)
        }

      case uexp =>
        throw ParseFailure("{", uexp.toString, path)
    }
  }
  private def parseSign(s: Input): (Option[Char], Input) = {
    getChar(s) match {
      case sign(sChar) => Some(sChar) -> s.next
      case _           => None        -> s
    }
  }

  implicit class SourceOps[A](s: F[Option[A]]) {
    def flatFold[B](f: A => F[B])(orElse: () => F[B]): F[B] = {
      s.flatMap {
        case Some(a) => f(a)
        case None    => orElse()
      }
    }
  }

  private case class Part1(part1: StringBuilder, sep: Option[Char], cont: Input)
  private case class Part2(part2: StringBuilder, sep: Option[Char], cont: Input)

  private def consumeTillTermination[A](s: Input)(term: ExpectedTerminator, f: Input => A)(
      implicit p: Vector[PPath]): A = {
    getChar(s) match {
      case c if term.matchChar(c) => f(s)
      case uexp                   => throw ParseFailure(term.toString, uexp.toString, p)
    }
  }

  private def parseNum1(s: Input)(term: ExpectedTerminator)(implicit p: Vector[PPath]): Part1 = {

    def recurse(acc: StringBuilder, s: Input): Part1 = {
      val next = s.next

      getCharOpt(s) match {
        case Some(c) =>
          c match {
            case digit(d)               => recurse(acc.append(d), next)
            case t if term.matchChar(t) => Part1(acc, None, s)
            case '.'                    => Part1(acc, Some('.'), next)
            case 'E'                    => Part1(acc, Some('e'), next)
            case 'e'                    => Part1(acc, Some('e'), next)
            case whitespace(_) =>
              consumeTillTermination(next)(term, n => Part1(acc, None, n))
            case u =>
              throw ParseFailure(s"Digit or $term", u.toString, p)
          }
        case None =>
          if (term == End && acc.nonEmpty) {
            Part1(acc, None, s)
          } else {
            throw ParseFailure.termination
          }
      }
    }

    val (maybe, next) = parseSign(s)
    val stringBuilder = new StringBuilder
    maybe.foreach(stringBuilder.append)
    recurse(stringBuilder, next)
  }

  private def parseNum2(s: Input, p1Sep: Char)(term: ExpectedTerminator)(
      implicit p: Vector[PPath]): Part2 = {
    def recurse(acc: StringBuilder, s: Input): Part2 = {
      val next = s.next
      getCharOpt(s) match {
        case Some(c) =>
          c match {
            case digit(d)                    => recurse(acc.append(d), next)
            case t if term.matchChar(t)      => Part2(acc, None, s)
            case exponent(e) if p1Sep == '.' => Part2(acc, Some(e), next)
            case whitespace(_) =>
              consumeTillTermination(next)(term, n => Part2(acc, None, n))
            case u =>
              throw ParseFailure(s"Digit or $term", u.toString, p)
          }
        case None =>
          if (term == End && acc.nonEmpty) {
            Part2(acc, None, s)
          } else {
            throw ParseFailure.termination
          }
      }
    }

    recurse(StringBuilder.newBuilder, s)
  }

  private def parseNum3(s: Input)(term: ExpectedTerminator)(
      implicit path: Vector[PPath]): (StringBuilder, Input) = {
    def recurse(acc: StringBuilder, s: Input): (StringBuilder, Input) = {
      val next = s.next
      getCharOpt(s) match {
        case Some(c) =>
          c match {
            case digit(d)               => recurse(acc.append(d), next)
            case t if term.matchChar(t) => acc -> s
            case whitespace(_) =>
              consumeTillTermination(next)(term, n => acc -> n)
            case u =>
              throw ParseFailure(s"Digit or $term", u.toString, path)
          }
        case None =>
          if (term == End && acc.nonEmpty) {
            acc -> s
          } else {
            throw ParseFailure.termination
          }
      }
    }

    val (maybe, next) = parseSign(s)
    val stringBuilder = new StringBuilder
    maybe.foreach(stringBuilder.append)
    recurse(stringBuilder, next)
  }

  implicit val ParseApp: Applicative[Parse] = new Applicative[Parse] {
    override def pure[A](x: A): Parse[A] = { path => src =>
      ME.pure(x -> src)
    }
    override def ap[A, B](ff: Parse[A => B])(fa: Parse[A]): Parse[B] = { path => src =>
      for {
        pair1           <- ff(path)(src): F[(A => B, Input)]
        (fn, _)         = pair1
        pair2           <- fa(path)(src): F[(A, Input)]
        (a, restSource) = pair2
      } yield {
        fn(a) -> restSource
      }
    }
  }

  def parseOneOf[I](oneOf: NonEmptyMap[String, Lazy[Parse[I]]]): Parse[I] = { path => src =>
    (parseObj(discriminatorField, parseString)(path)(src): F[(String, Input)]).flatMap {
      case (key, _) =>
        oneOf(key) match {
          case Some(parseFn) => parseFn.value(path)(src)
          case None =>
            ME.raiseError[(I, Input)](ParseFailure(s"Cannot parse object with type=$key"))
        }
    }
  }

  def parseOptional[I](parse: Parse[I]): Parse[Option[I]] = { path => src =>
    (parse(path)(src): F[(I, Input)])
      .map[(Option[I], Input)] {
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

object JsonArrayParse {

  type Input           = (Array[Char], Int)
  type Output[F[_], A] = F[(A, Input)]

  implicit class PointerOps(input: Input) {
    def isFollowedBy(str: String): (Boolean, Int) = {
      val (fullSource, i) = input
      val slice           = fullSource.slice(i, i + str.length)
      slice.sameElements(str.toCharArray) -> (i + str.length)
    }

    def next: Input         = move(1)
    def move(i: Int): Input = (input._1, input._2 + i)

    def pointTo(i: Int): Input = input._1 -> i
  }
}
