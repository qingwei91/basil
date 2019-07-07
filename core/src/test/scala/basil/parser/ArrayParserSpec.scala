package basil.parser

import basil.data.ParseFailure
import basil.parser.ArrayParserSpec._
import basil.parser.implicits.ArrayParse

class ArrayParserSpec extends ParseSpec[Input, Output] {
  override implicit val parser: JsonArrayParse[Either[ParseFailure, ?]] = ArrayParse

  override def liftF(charArr: Array[Char]): Input = charArr -> 0

  override def getLast[A](f: Output[A]): A = f.fold[A](
    failure => throw failure,
    result => result._1
  )
}

object ArrayParserSpec {
  type Input     = (Array[Char], Int)
  type Output[A] = Either[ParseFailure, (A, Input)]
}
