package basil.parser

import basil.typeclass.EffStack
import basil.typeclass.instances._
import cats.instances.list._
import cats.instances.try_._

import scala.util.Try

object implicits {

  implicit object ListJsonParser extends JsonParse[EffStack[Try, List, ?]]

}
