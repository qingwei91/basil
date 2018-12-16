package basil.typeclass

trait Iso[F[_], A, B] {
  def toB(a: F[A]): F[B]
  def toA(b: F[B]): F[A]
}
