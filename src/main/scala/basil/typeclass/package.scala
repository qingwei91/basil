package basil

package object typeclass {
  type EStack[E[_], F[_], A] = E[F[A]]
}
