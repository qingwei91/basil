package basil.parser

import matryoshka.data.Fix
import scalaz._
import scalaz.syntax.applicative._

sealed trait ParseOps[+A]

case object GetString                      extends ParseOps[Nothing]
case object GetNum                         extends ParseOps[Nothing]
case object GetBool                        extends ParseOps[Nothing]
case class GetNullable[A](ops: A)          extends ParseOps[A]
case class GetN[A](n: Int, next: A)        extends ParseOps[A]
case class GetKey[A](key: String, next: A) extends ParseOps[A]

object ParseOps {
  implicit val traversable: Traverse[ParseOps] = new Traverse[ParseOps] {
    override def traverseImpl[G[_], A, B](fa: ParseOps[A])(f: A => G[B])(
        implicit AG: Applicative[G]): G[ParseOps[B]] = {
      fa match {
        case GetString      => AG.point(GetString)
        case GetNum         => AG.point(GetNum)
        case GetBool        => AG.point(GetBool)
        case GetNullable(a) => f(a).map(GetNullable.apply)
        case GetN(n, a)     => f(a).map(b => GetN(n, b))
        case GetKey(k, a)   => f(a).map(b => GetKey(k, b))
      }
    }
  }
  type ParseOpsF    = ParseOps[Fix[ParseOps]]
  type StringEff[A] = Const[String, A]
  implicit val showRecursiveOps: Show[ParseOpsF] = Show.shows[ParseOpsF] {
    case GetString => GetString.toString
    case GetNum    => GetNum.toString
    case GetBool   => GetBool.toString
    case GetNullable(a) =>
      s"""GetNullable {
        |  ${showRecursiveOps.shows(a.unFix)}
        |}""".stripMargin
    case GetN(n, a) =>
      s"""
         |GetN($n) {
         |  ${showRecursiveOps.shows(a.unFix)}
         |}
       """.stripMargin
    case GetKey(k, a) =>
      s"""
         |GetKey($k) {
         |  ${showRecursiveOps.shows(a.unFix)}
         |}
       """.stripMargin
  }
}
