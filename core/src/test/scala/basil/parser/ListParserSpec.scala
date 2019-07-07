package basil.parser

import basil.parser.ListParserSpec._
import basil.typeclass.instances._

import scala.util.Success
class ListParserSpec extends ParseSpec[Input, Output] {
  override implicit val parser: JsonStreamParse[TryF[List, ?]] = implicits.ListJsonParser

  override def liftF(charArr: Array[Char]): TryF[List, Char] = Success(List(charArr: _*))

  override def getLast[A](f: Output[A]): A = f.get.last._1
}

object ListParserSpec {
  type Input     = TryF[List, Char]
  type Output[A] = TryF[List, (A, Input)]
}
