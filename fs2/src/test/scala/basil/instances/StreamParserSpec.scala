package basil.instances

import basil.instances.fs2Instances.StreamJsonParser
import basil.parser.{JsonStreamParse, ParseSpec}
import cats.effect.IO
import fs2.Stream
import StreamParserSpec._
class StreamParserSpec extends ParseSpec[Input, Output] {
  override implicit val parser: JsonStreamParse[Stream[IO, ?]] = StreamJsonParser

  override def liftF(charArr: Array[Char]): Stream[IO, Char] = Stream(charArr: _*)

  override def getLast[A](f: Output[A]): A = f.compile.lastOrError.unsafeRunSync()._1
}

object StreamParserSpec {
  type Input     = Stream[IO, Char]
  type Output[A] = Stream[IO, (A, Input)]
}
