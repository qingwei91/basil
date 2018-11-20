package basil.parser

import basil.typeclass.instances._
import cats.instances.list._

import scala.util.Success

class ListParserSpec extends ParseSpec[TryList] {
  override implicit val parser: JsonParse[TryList] = implicits.ListJsonParser

  override def liftF(charArr: Array[Char]): TryList[Char] = Success(List(charArr: _*))

  override def getLast[A](f: TryList[A]): A = f.get.last
}
