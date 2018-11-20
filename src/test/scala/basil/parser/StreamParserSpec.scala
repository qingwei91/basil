package basil.parser

import cats.effect.IO
import fs2.Stream

class StreamParserSpec extends ParseSpec[Stream[IO, ?]] {
  override implicit val parser: JsonParse[Stream[IO, ?]] = implicits.StreamJsonParser

  override def liftF(charArr: Array[Char]): Stream[IO, Char] = Stream(charArr: _*)

  override def getLast[A](f: Stream[IO, A]): A = f.compile.lastOrError.unsafeRunSync()
}
