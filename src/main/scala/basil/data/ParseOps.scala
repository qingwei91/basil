package basil.data

import cats.data.Const
import cats.syntax.functor._
import cats.{Applicative, Eval, Show, Traverse}
import schemes._

sealed trait ParseOps[+A]

case object GetString                       extends ParseOps[Nothing]
case object GetBool                         extends ParseOps[Nothing]
case class GetNum(term: ExpectedTerminator) extends ParseOps[Nothing]
case class GetNullable[A](ops: A)           extends ParseOps[A]
case class GetN[A](n: Int, next: A)         extends ParseOps[A]
case class GetKey[A](key: String, next: A)  extends ParseOps[A]

object ParseOps {
  implicit val traversable: Traverse[ParseOps] = new Traverse[ParseOps] {
    override def traverse[G[_], A, B](fa: ParseOps[A])(f: A => G[B])(
        implicit AG: Applicative[G]): G[ParseOps[B]] = {
      fa match {
        case GetString      => AG.point(GetString)
        case g: GetNum      => AG.point(g)
        case GetBool        => AG.point(GetBool)
        case GetNullable(a) => f(a).map(GetNullable.apply)
        case GetN(n, a)     => f(a).map(b => GetN(n, b))
        case GetKey(k, a)   => f(a).map(b => GetKey(k, b))
      }
    }

    override def foldLeft[A, B](fa: ParseOps[A], b: B)(f: (B, A) => B): B = {
      fa match {
        case GetString      => b
        case GetNum(_)      => b
        case GetBool        => b
        case GetNullable(a) => f(b, a)
        case GetN(_, a)     => f(b, a)
        case GetKey(_, a)   => f(b, a)
      }
    }

    override def foldRight[A, B](fa: ParseOps[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case GetString      => lb
      case GetNum(_)      => lb
      case GetBool        => lb
      case GetNullable(a) => f(a, lb)
      case GetN(_, a)     => f(a, lb)
      case GetKey(_, a)   => f(a, lb)
    }
  }

  type ParseOpsF    = ParseOps[Fix[ParseOps]]
  type StringEff[A] = Const[String, A]
  implicit val showRecursiveOps: Show[ParseOpsF] = Show.show[ParseOpsF] {
    case GetString => GetString.toString
    case g: GetNum => g.toString
    case GetBool   => GetBool.toString
    case GetNullable(a) =>
      s"""GetNullable {
        |  ${showRecursiveOps.show(a.unfix)}
        |}""".stripMargin
    case GetN(n, a) =>
      s"""
         |GetN($n) {
         |  ${showRecursiveOps.show(a.unfix)}
         |}
       """.stripMargin
    case GetKey(k, a) =>
      s"""
         |GetKey($k) {
         |  ${showRecursiveOps.show(a.unfix)}
         |}
       """.stripMargin
  }
}
