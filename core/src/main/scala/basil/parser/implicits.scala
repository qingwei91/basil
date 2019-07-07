package basil.parser

import basil.data.ParseFailure
import basil.typeclass.EffStack
import basil.typeclass.instances._
import cats.instances.list._
import cats.instances.try_._
import cats.instances.either._

import scala.util.Try

object implicits {

  implicit object ListJsonParser extends JsonStreamParse[EffStack[Try, List, ?]]

  implicit object ArrayParse extends JsonArrayParse[Either[ParseFailure, ?]]()

}
