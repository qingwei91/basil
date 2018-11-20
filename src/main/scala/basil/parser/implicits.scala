package basil.parser

import basil.typeclass.instances._
import cats.effect.IO
import cats.instances.list._
import fs2.Stream

object implicits {
  implicit object ListJsonParser   extends JsonParse[TryList]
  implicit object StreamJsonParser extends JsonParse[Stream[IO, ?]]
}
