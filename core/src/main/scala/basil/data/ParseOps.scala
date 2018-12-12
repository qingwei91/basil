package basil.data

import cats.free.FreeApplicative
import cats.~>

/**
  * Potentially recursive tree to represent data that's needed
  * from json
  *
  * todo: support sequence
  */
sealed trait ParseOps[+F[_], I]

case object GetString extends ParseOps[Nothing, String]

case object GetBool extends ParseOps[Nothing, Boolean]

final case class GetNum(term: ExpectedTerminator) extends ParseOps[Nothing, Double]

final case class GetMultiple[F[_], I](allOf: FreeApplicative[ParseOps[F, ?], I])
    extends ParseOps[F, I]

final case class GetN[F[_], I](n: Int, next: F[I]) extends ParseOps[F, I]

final case class GetKey[F[_], I](key: String, next: F[I]) extends ParseOps[F, I]

/**
  * Optional is a special case as it changes the shape of the type from I => Option[I]
  * This causes issue when you have nesting Optional value, like Some(Some(Some(None)))
  * To mitigate this problem, we use a specialized `GetOptFlat` that know how to flatten nested Option
  * Another possible option is to do runtime type check with the algebra, this approach is less clean as we cant rely on compiler
  */
final case class GetOpt[F[_], I](next: F[I])             extends ParseOps[F, Option[I]]
final case class GetOptFlat[F[_], I](next: F[Option[I]]) extends ParseOps[F, Option[I]]

object ParseOps {
  implicit val ParseOpsHFunctor: HFunctor[ParseOps] = new HFunctor[ParseOps] {
    override def hfmap[M[_], N[_]](nt: M ~> N): ParseOps[M, ?] ~> ParseOps[N, ?] = {
      new (ParseOps[M, ?] ~> ParseOps[N, ?]) { self =>
        override def apply[A](fa: ParseOps[M, A]): ParseOps[N, A] = {
          fa match {
            case GetString               => GetString
            case GetBool                 => GetBool
            case num: GetNum             => num
            case getN: GetN[M, A]        => GetN(getN.n, nt(getN.next))
            case getK: GetKey[M, A]      => GetKey(getK.key, nt(getK.next))
            case getM: GetMultiple[M, A] => GetMultiple(getM.allOf.compile(self))

            case getO: GetOpt[M, a] with ParseOps[M, A] =>
              GetOpt[N, a](nt(getO.next)).asInstanceOf[ParseOps[N, A]]

            case getOptFlat: GetOptFlat[M, a] with ParseOps[M, A] =>
              GetOptFlat[N, a](nt(getOptFlat.next)).asInstanceOf[ParseOps[N, A]]
          }
        }
      }
    }
  }
}
