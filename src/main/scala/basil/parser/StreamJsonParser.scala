package basil.parser

import basil.typeclass.instances._
import cats.effect.IO
import fs2.Stream
import org.json4s.JsonAST._

object StreamJsonParser extends JsonParse[Stream[IO, ?], JValue]
