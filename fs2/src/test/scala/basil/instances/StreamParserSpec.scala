package basil.instances

import basil.instances.fs2Instances.StreamJsonParser
import basil.parser.{JsonParse, ParseSpec}
import cats.effect.IO
import fs2.Stream

class StreamParserSpec extends ParseSpec[Stream[IO, ?]] {
  override implicit val parser: JsonParse[Stream[IO, ?]] = StreamJsonParser

  override def liftF(charArr: Array[Char]): Stream[IO, Char] = Stream(charArr: _*)

  override def getLast[A](f: Stream[IO, A]): A = f.compile.lastOrError.unsafeRunSync()
}
