package basil.parser

import basil.typeclass.instances._
import cats.effect.IO
import cats.instances.list._
import fs2.Stream
import org.json4s.JsonAST.JValue

object implicits {
  implicit object ListJsonParser   extends JsonParse[TryList, JValue]
  implicit object StreamJsonParser extends JsonParse[Stream[IO, ?], JValue]
}
