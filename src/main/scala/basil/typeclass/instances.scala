package basil.typeclass

import basil.data.ParseFailure
import cats.effect.IO
import cats.implicits._
import cats.kernel.Monoid
import cats._
import fs2.Stream

import scala.util.{Failure, Success, Try}

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

  type TryF[F[_], A] = Try[F[A]]

  implicit def tryFMonoid[F[_], A](implicit monoid: Monoid[F[A]]): Monoid[TryF[F, A]] =
    new Monoid[TryF[F, A]] {
      override def empty: TryF[F, A] = Success(monoid.empty)

      override def combine(x: TryF[F, A], y: TryF[F, A]): TryF[F, A] = {
        (x, y) match {
          case (Success(x), Success(y)) => Success(monoid.combine(x, y))
          case (f @ Failure(_), _)      => f
          case (_, f @ Failure(_))      => f
        }
      }
    }

  implicit def tryAMonadError[F[_]: Monad: Traverse]: MonadError[TryF[F, ?], ParseFailure] =
    new MonadError[TryF[F, ?], ParseFailure] {
      override def raiseError[A](e: ParseFailure): TryF[F, A] = Failure(e)
      override def handleErrorWith[A](fa: TryF[F, A])(f: ParseFailure => TryF[F, A]): TryF[F, A] =
        fa.recoverWith {
          case e: ParseFailure => f(e)
        }
      override def flatMap[A, B](fa: TryF[F, A])(f: A => TryF[F, B]): TryF[F, B] = fa.flatMap {
        inner =>
          inner.traverse[Try, F[B]](a => f(a)).map(_.flatten)

      }
      override def tailRecM[A, B](a: A)(f: A => TryF[F, Either[A, B]]): TryF[F, B] = {

        def loop(i: TryF[F, Either[A, B]]): TryF[F, B] = {
          i match {
            case Success(v) =>
              v.traverse[Try, F[B]] {
                  case Right(b) => Success(Monad[F].pure(b))
                  case Left(a)  => loop(f(a))
                }
                .map(_.flatten)
            case Failure(e) => Failure(e)
          }
        }

        loop(f(a))
      }

      override def pure[A](x: A): TryF[F, A] = Success(Monad[F].pure(x))
    }
}
