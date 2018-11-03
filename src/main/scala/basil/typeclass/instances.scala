package basil.typeclass

import basil.parser.ParseFailure
import cats.effect.IO
import cats.kernel.Monoid
import cats.{Monad, MonadError}
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

  implicit val streamCons: Cons[Stream[IO, ?]] = new Cons[Stream[IO, ?]] {
    override def cons[E](cols: Stream[IO, E], e: E): Stream[IO, E] = cols.cons1(e)
  }

  type TryList[A] = Try[List[A]]
  implicit def unsafeListMonadError(
      implicit listMonad: Monad[List]): MonadError[TryList, ParseFailure] =
    new MonadError[TryList, ParseFailure] {
      override def flatMap[A, B](fa: TryList[A])(f: A => TryList[B]): TryList[B] = {
        fa.flatMap { la =>
          la.foldLeft[TryList[B]](Success(Nil)) {
            case (Success(accB), a) => f(a).map(lb => accB ::: lb)
            case (failure, _)       => failure
          }
        }
      }

      override def tailRecM[A, B](a: A)(f: A => TryList[Either[A, B]]): TryList[B] = {
        def expand(aOrB: List[Either[A, B]], acc: List[B]): TryList[B] = {
          aOrB match {
            case h :: tail =>
              h match {
                case Right(b) => expand(tail, b :: acc)
                case Left(a) =>
                  f(a) match {
                    case Success(value)     => expand(value, acc)
                    case Failure(exception) => Failure(exception)
                  }
              }
            case Nil => Success(acc.reverse)
          }

        }

        f(a).flatMap(abs => expand(abs, Nil))
      }

      override def raiseError[A](e: ParseFailure): TryList[A] = Failure(e)

      override def handleErrorWith[A](fa: TryList[A])(f: ParseFailure => TryList[A]): TryList[A] =
        fa.recoverWith {
          case parseFailure: ParseFailure => f(parseFailure)
          case other                      => Failure(other)
        }

      override def pure[A](x: A): TryList[A] = Success(listMonad.pure(x))
    }

  implicit def tryListMonoid[A](implicit listMonoid: Monoid[List[A]]): Monoid[TryList[A]] =
    new Monoid[TryList[A]] {
      override def empty: TryList[A] = Success(Nil)

      override def combine(x: TryList[A], y: TryList[A]): TryList[A] = {
        (x, y) match {
          case (Success(x), Success(y)) => Success(listMonoid.combine(x, y))
          case (f @ Failure(_), _)      => f
          case (_, f @ Failure(_))      => f
        }
      }
    }

  implicit val tryListCons: Cons[TryList] = new Cons[TryList] {
    override def cons[E](cols: TryList[E], e: E): TryList[E] = cols.map { ls =>
      e :: ls
    }
  }
}
