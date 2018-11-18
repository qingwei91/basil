package basil.data

import basil.data.HFix.HAlgebra
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

object HFunctor {
  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G
}

case class HFix1[F[_[_], _], I](unfix: F[HFix1[F, ?], I])
object HFix1 {
  
}
