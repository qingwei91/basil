package basil.parser

import basil.data.ParseFailure
import basil.parser.StringParserSpec._
import basil.parser.implicits._

class StringParserSpec extends ParseSpec[Input, Output] {
  override implicit val parser: JsonArrayParse[Either[ParseFailure, ?], String] = StringParse

  override def liftF(charArr: Array[Char]): Input = new String(charArr) -> 0

  override def getLast[A](f: Output[A]): A = f.fold[A](
    failure => throw failure,
    result => result
  )
}

object StringParserSpec {
  type Input     = (String, Int)
  type Output[A] = Either[ParseFailure, A]
}
