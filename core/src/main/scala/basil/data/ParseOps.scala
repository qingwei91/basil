package basil.data

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import cats.{Functor, ~>}

/**
  * Potentially recursive tree to represent data we want from json
  */
sealed trait ParseOps[+F[_], +I]

case object GetString extends ParseOps[Nothing, String]

case object GetBool extends ParseOps[Nothing, Boolean]

final case class GetNum(term: ExpectedTerminator) extends ParseOps[Nothing, Double]

final case class GetN[F[_], I](n: Int, next: F[I]) extends ParseOps[F, I]

final case class GetKey[F[_], I](key: String, next: F[I]) extends ParseOps[F, I]

/**
  * Optional is a special case as it changes the shape of the type from I => Option[I]
  * This causes issue when you have nesting Optional value, like Some(Some(Some(None)))
  * Currently this issue is handled by manual flattening in interpreter
  */
final case class GetOpt[F[_], I](next: F[I]) extends ParseOps[F, Option[I]]

final case class GetSum[F[_], I](oneOf: NonEmptyList[F[I]]) extends ParseOps[F, I]

final case class GetProduct[F[_], I](allOf: FreeApplicative[ParseOps[F, ?], I])
    extends ParseOps[F, I]

// this feel like a hack, I need the ability to map over type I
// but I dont know how to achieve that without introducing yet another
// case class
final case class Mapped[F[_], H, I](fi: F[H], fn: H => I) extends ParseOps[F, I]

object ParseOps {
  implicit val ParseOpsHFunctor: HFunctor[ParseOps] = new HFunctor[ParseOps] {
    override def hfmap[M[_], N[_]](nt: M ~> N): ParseOps[M, ?] ~> ParseOps[N, ?] = {
      new (ParseOps[M, ?] ~> ParseOps[N, ?]) { self =>
        override def apply[A](fa: ParseOps[M, A]): ParseOps[N, A] = {
          fa match {
            case GetString              => GetString
            case GetBool                => GetBool
            case num: GetNum            => num
            case getN: GetN[M, A]       => GetN(getN.n, nt(getN.next))
            case getK: GetKey[M, A]     => GetKey(getK.key, nt(getK.next))
            case getM: GetProduct[M, A] => GetProduct(getM.allOf.compile(self))
            case getS: GetSum[M, A]     => GetSum(getS.oneOf.map(a => nt(a)))

            case getO: GetOpt[M, a] with ParseOps[M, A] =>
              GetOpt[N, a](nt(getO.next)).asInstanceOf[ParseOps[N, A]]

            case mapped: Mapped[M, h, A] => Mapped(nt(mapped.fi), mapped.fn)
          }
        }
      }
    }
  }

  // it's interesting that we can only get functor with HFix[ParseOps, ?]
  // but not ParseOps
  // why???
  implicit val NestedParseOpsFunctor: Functor[HFix[ParseOps, ?]] = new Functor[HFix[ParseOps, ?]] {
    override def map[A, B](fa: HFix[ParseOps, A])(f: A => B): HFix[ParseOps, B] = {
      fa.unfix match {
        case Mapped(fi, fn) => HFix(Mapped(fi, fn.andThen(f)))
        case _              => HFix(Mapped(fa, f))
      }
    }
  }

}
