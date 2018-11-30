package basil

package object typeclass {
  type EffStack[E[_], F[_], A] = E[F[A]]
}
