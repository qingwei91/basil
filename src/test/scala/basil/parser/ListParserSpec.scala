package basil.parser

import basil.typeclass.instances._
import cats.instances.list._

import scala.util.Success

class ListParserSpec extends ParseSpec[TryF[List, ?]] {
  override implicit val parser: JsonParse[TryF[List, ?]] = implicits.ListJsonParser

  override def liftF(charArr: Array[Char]): TryF[List, Char] = Success(List(charArr: _*))

  override def getLast[A](f: TryF[List, A]): A = f.get.last
}
