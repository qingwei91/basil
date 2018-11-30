package basil.data

import cats.~>

/**
  * Copied from `xenomorph` project
  *
  * Core concept: F[_[_], _] => ((* -> *) -> *) -> *
  */
/**
  * Higher functor provides ability to
  * @tparam F
  */
trait HFunctor[F[_[_], _]] {
  def hfmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
}

/**
  * In  :: fT => T
  * Out :: T => fT
  */
case class HFix[F[_[_], _], I](unfix: F[HFix[F, ?], I])
object HFix {
  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G

  final implicit class HFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: HFunctor[F]) {
    def hfmap[N[_]](nt: M ~> N): F[N, A] = F.hfmap(nt)(fa)
  }

  def cata[F[_[_], _], I, G[_]](hfix: HFix[F, I], alg: HAlgebra[F, G])(
      implicit HFunctor: HFunctor[F]): G[I] = {
    val unpacked: F[HFix[F, ?], I] = hfix.unfix
    val r = unpacked.hfmap(
      new (HFix[F, ?] ~> G) {
        override def apply[A](fa: HFix[F, A]): G[A] = {
          cata[F, A, G](fa, alg)
        }
      }
    )
    alg(r)
  }
}
