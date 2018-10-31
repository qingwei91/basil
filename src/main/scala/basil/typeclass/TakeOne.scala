package basil.typeclass

import cats.effect.IO
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Eq, Functor, Monad}

trait TakeOne[Source[_]] {
  def take1[Element](src: Source[Element]): Source[(Element, Source[Element])]

  def take1Opt[Element](src: Source[Element]): Source[(Option[Element], Source[Element])]

  def peek1[Element](src: Source[Element])(
      implicit Functor: Functor[Source],
      cons: Cons[Source]): Source[(Element, Source[Element])] = {
    take1(src).map {
      case (ele, next) => ele -> cons.cons(next, ele)
    }
  }

  def drop1[Element](src: Source[Element])(implicit monad: Monad[Source]): Source[Element] =
    take1(src).flatMap(_._2)

  def isFollowedBy[Element](src: Source[Element])(expected: List[Element])(
      implicit EQ: Eq[Element],
      monad: Monad[Source],
      Cons: Cons[Source]): Source[(Boolean, Source[Element])] = {
    expected.headOption match {
      case Some(h) =>
        take1(src).flatMap {
          case (ele, next) if ele === h => isFollowedBy(next)(expected.tail)
          case (ele, next)              => monad.pure(false -> Cons.cons(next, ele))
        }
      case None => monad.pure(true -> src)
    }

  }

  def accUntil[Element](stream: Source[Element])(until: Element => Boolean)(
      implicit monad: Monad[Source]): Source[(Vector[Element], Source[Element])] = {
    def recurse(stream: Source[Element])(
        acc: Vector[Element]): Source[(Vector[Element], Source[Element])] = {
      take1(stream).flatMap {
        case (ele, next) if until(ele) => monad.pure(acc -> next)
        case (ele, next)               => recurse(next)(acc :+ ele)
      }
    }

    recurse(stream)(Vector.empty)
  }
}

trait TakeOneSyntax {
  implicit def syntaxTakeOne[E, Src[_]](src: Src[E])(implicit t: TakeOne[Src],
                                                     c: Cons[Src]): TakeOps[E, Src] =
    new TakeOps(src)
}

object TakeOne extends TakeOneSyntax {
  import fs2.{Pull, Stream}

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
}

final class TakeOps[Element, Source[_]](src: Source[Element])(implicit take: TakeOne[Source],
                                                              cons: Cons[Source]) {

  def take1: Source[(Element, Source[Element])]            = take.take1(src)
  def take1Opt: Source[(Option[Element], Source[Element])] = take.take1Opt(src)

  def peek1(implicit Functor: Functor[Source]): Source[(Element, Source[Element])] = take.peek1(src)

  def accUntil(until: Element => Boolean)(
      implicit monad: Monad[Source]): Source[(Vector[Element], Source[Element])] =
    take.accUntil(src)(until)

  def isFollowedBy(expected: List[Element])(
      implicit EQ: Eq[Element],
      monad: Monad[Source]): Source[(Boolean, Source[Element])] =
    take.isFollowedBy(src)(expected)

  def drop1(implicit monad: Monad[Source]): Source[Element] = take.drop1(src)
}
