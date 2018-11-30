package basil.typeclass

import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Eq, Functor, Monad}

/**
  * Typeclass to provide capability of taking 1 element from a data
  * source
  *
  * User needs to implement 2 methods
  *   - take1
  *   - take1Opt
  * @tparam Source
  */
trait TakeOne[Source[_]] {

  /**
    * takes an element from Source and return the Element with
    * the rest of the Data without escaping the Source context
    *
    * It will return empty source if input source is empty,
    * the emptiness is implied and not captured in part of the type
    *
    * essentially List[Int]() and List[String]() are equivalent
    */
  def take1[Element](src: Source[Element]): Source[(Element, Source[Element])]

  /**
    * takes an element from Source and return the element with
    * the rest of the Data
    *
    * It will return None and Empty source if input source is empty,
    * the main difference of this method versus `take1` is that
    * `take1Opt` allows caller to handle emptiness within Source
    * context
    *
    * This is useful when you want to tranform empty Source into
    * something else
    */
  def take1Opt[Element](src: Source[Element]): Source[(Option[Element], Source[Element])]

  /**
    * returns the 1st element from the source without consuming
    * the data source
    */
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
}

trait TakeOneSyntax {
  implicit def syntaxTakeOne[E, Src[_]](src: Src[E])(implicit t: TakeOne[Src],
                                                     c: Cons[Src]): TakeOps[E, Src] =
    new TakeOps(src)
}

object TakeOne extends TakeOneSyntax {

  def apply[F[_]](implicit t: TakeOne[F]): TakeOne[F] = t

  implicit val listTakeOne: TakeOne[List] = new TakeOne[List] {

    override def take1[Element](src: List[Element]): List[(Element, List[Element])] = {
      src match {
        case h :: t => List(h -> t)
        case Nil    => Nil
      }
    }

    override def take1Opt[Element](src: List[Element]): List[(Option[Element], List[Element])] = {
      src match {
        case h :: t => List(Some(h) -> t)
        case Nil    => List(None    -> Nil)
      }
    }
  }

  implicit def stackTakeOne[E[_]: Applicative, F[_]: TakeOne: Functor]: TakeOne[EffStack[E, F, ?]] =
    new TakeOne[EffStack[E, F, ?]] {
      override def take1[Element](
          src: EffStack[E, F, Element]): EffStack[E, F, (Element, EffStack[E, F, Element])] = {
        Functor[E].map(src) { fa =>
          TakeOne[F].take1(fa).map {
            case (e, fe) => e -> Applicative[E].pure(fe)
          }
        }
      }

      override def take1Opt[Element](src: EffStack[E, F, Element])
        : EffStack[E, F, (Option[Element], EffStack[E, F, Element])] = {
        Functor[E].map(src) { fa =>
          TakeOne[F].take1Opt(fa).map {
            case (e, fe) => e -> Applicative[E].pure(fe)
          }
        }
      }
    }
}

final class TakeOps[Element, Source[_]](src: Source[Element])(implicit take: TakeOne[Source],
                                                              cons: Cons[Source]) {

  def take1: Source[(Element, Source[Element])]            = take.take1(src)
  def take1Opt: Source[(Option[Element], Source[Element])] = take.take1Opt(src)

  def peek1(implicit Functor: Functor[Source]): Source[(Element, Source[Element])] = take.peek1(src)

  def isFollowedBy(expected: List[Element])(
      implicit EQ: Eq[Element],
      monad: Monad[Source]): Source[(Boolean, Source[Element])] =
    take.isFollowedBy(src)(expected)

  def drop1(implicit monad: Monad[Source]): Source[Element] = take.drop1(src)
}
