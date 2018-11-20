package basil.data

import cats.~>

/**
  * Potentially recursive tree to represent data that's needed
  * from json
  *
  * todo: support sequence
  */
sealed trait ParseOps[+F[_], I]

case object GetString                             extends ParseOps[Nothing, String]
case object GetBool                               extends ParseOps[Nothing, Boolean]
final case class GetNum(term: ExpectedTerminator) extends ParseOps[Nothing, Double]
final case class GetNullable[F[_], I](ops: F[I]) extends ParseOps[F, Option[I]] {
  def toG[G[_]](nt: F ~> G): GetNullable[G, I] = GetNullable(nt(ops))
}
final case class GetN[F[_], I](n: Int, next: F[I])        extends ParseOps[F, I]
final case class GetKey[F[_], I](key: String, next: F[I]) extends ParseOps[F, I]

object ParseOps {
  implicit val ParseOpsHFunctor: HFunctor[ParseOps] = new HFunctor[ParseOps] {
    override def hfmap[M[_], N[_]](nt: M ~> N): ParseOps[M, ?] ~> ParseOps[N, ?] = {
      new (ParseOps[M, ?] ~> ParseOps[N, ?]) {
        override def apply[A](fa: ParseOps[M, A]): ParseOps[N, A] = {
          fa match {
            case g: GetNullable[M, a] with ParseOps[M, A] =>
              // todo: can we avoid casting ?
              GetNullable(nt(g.ops)).asInstanceOf[ParseOps[N, A]]

            case getN: GetN[M, A]   => GetN(getN.n, nt(getN.next))
            case getK: GetKey[M, A] => GetKey(getK.key, nt(getK.next))
            case GetString          => GetString
            case GetBool            => GetBool
            case num: GetNum        => num
          }
        }
      }
    }
  }
}
