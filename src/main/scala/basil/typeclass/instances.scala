package basil.typeclass

import basil.parser.ParseFailure
import cats.effect.IO
import cats.{Monad, MonadError}
import fs2.Stream

object instances {
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

  implicit val streamCons: Cons[Stream[IO, ?]] = new Cons[Stream[IO, ?]] {
    override def cons[E](cols: Stream[IO, E], e: E): Stream[IO, E] = cols.cons1(e)
  }
}
