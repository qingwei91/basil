package basil.parser

import basil.data.ParseFailure
import basil.typeclass.{ArrayCharLike, EffStack}
import basil.typeclass.instances._
import cats.instances.list._
import cats.instances.try_._
import cats.instances.either._

import scala.util.Try

object implicits {

  implicit object ListJsonParser extends JsonStreamParse[EffStack[Try, List, ?]]

  implicit object ArrayParse extends JsonArrayParse[Either[ParseFailure, ?], Array[Char]]

  implicit object StringParse extends JsonArrayParse[Either[ParseFailure, ?], String]

  implicit val arrayCharIsArrayChar: ArrayCharLike[Array[Char]] = new ArrayCharLike[Array[Char]] {
    override def apply(t: Array[Char], idx: Int): Char = t(idx)

    override def isDefinedAt(t: Array[Char], idx: Int): Boolean = t.isDefinedAt(idx)

    override def slice(t: Array[Char], from: Int, until: Int): Array[Char] = t.slice(from, until)

    override def sameElements(a: Array[Char], b: Array[Char]): Boolean = a.sameElements(b)

    override def formString(t: Array[Char]): String = new String(t)

    override def length(t: Array[Char]): Int = t.length
  }

  implicit val stringIsArrChar: ArrayCharLike[String] = new ArrayCharLike[String] {
    override def apply(t: String, idx: Int): Char = t(idx)

    override def isDefinedAt(t: String, idx: Int): Boolean = t.isDefinedAt(idx)

    override def slice(t: String, from: Int, until: Int): String = t.slice(from, until)

    override def sameElements(a: String, b: String): Boolean = a == b

    override def formString(t: String): String = t

    override def length(t: String): Int = t.size
  }
}
