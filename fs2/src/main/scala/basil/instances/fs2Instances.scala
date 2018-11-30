package basil.instances

import basil.data.ParseFailure
import basil.parser.JsonParse
import basil.typeclass.{Cons, TakeOne}
import cats.{Monad, MonadError}
import cats.effect.IO
import fs2.{Pull, Stream}

object fs2Instances {
  implicit object StreamJsonParser extends JsonParse[Stream[IO, ?]]

  implicit def streamTakeOne: TakeOne[Stream[IO, ?]] =
    new TakeOne[Stream[IO, ?]] {
      override def take1[C](src: Stream[IO, C]): Stream[IO, (C, Stream[IO, C])] = {
        src.pull.uncons1.flatMap {
          case Some((e, next)) => Pull.output1(e -> next)
          case None            => Pull.pure(())
        }.stream
      }

      override def take1Opt[C](src: Stream[IO, C]): Stream[IO, (Option[C], Stream[IO, C])] =
        src.pull.uncons1.flatMap {
          case Some((e, next)) => Pull.output1(Some(e) -> next)
          case None            => Pull.output1(None    -> Stream.empty)
        }.stream
    }

  implicit val streamCons: Cons[Stream[IO, ?]] = new Cons[Stream[IO, ?]] {
    override def cons[E](cols: Stream[IO, E], e: E): Stream[IO, E] = cols.cons1(e)
  }

  implicit def streamMonadError(
      implicit streamMonad: Monad[Stream[IO, ?]]): MonadError[Stream[IO, ?], ParseFailure] = {
    new MonadError[Stream[IO, ?], ParseFailure] {

      override def raiseError[A](e: ParseFailure): Stream[IO, A] = Stream.raiseError[IO](e)

      override def handleErrorWith[A](fa: Stream[IO, A])(
          f: ParseFailure => Stream[IO, A]): Stream[IO, A] =
        fa.handleErrorWith {
          case parseFailure: ParseFailure => f(parseFailure)
          case t                          => Stream.raiseError[IO](t)
        }

      override def pure[A](x: A): Stream[IO, A] = streamMonad.pure(x)

      override def flatMap[A, B](fa: Stream[IO, A])(f: A => Stream[IO, B]): Stream[IO, B] =
        streamMonad.flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => Stream[IO, Either[A, B]]): Stream[IO, B] =
        streamMonad.tailRecM(a)(f)
    }
  }
}
