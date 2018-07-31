package basil.parser

import basil.MyFix
import basil.api.OpsTree
import matryoshka.data.Fix
import scalaz._
import scalaz.syntax.applicative._

sealed trait ParseOps[+A]

case object GetString extends ParseOps[Nothing]
case object GetNum extends ParseOps[Nothing]
case object GetBool extends ParseOps[Nothing]
case class GetNullable[A](ops: A) extends ParseOps[A]

case class GetN[A](n: Int, next: A) extends ParseOps[A]

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

  implicit val strTpe: ResultType[GetString.type] =
    new ResultType[GetString.type] {
      type R = String
    }

  implicit val numTpe: ResultType[GetNum.type] = new ResultType[GetNum.type] {
    type R = Double
  }

  implicit val boolTpe: ResultType[GetBool.type] =
    new ResultType[GetBool.type] {
      type R = Boolean
    }

  implicit def nullableTpe[A](
      implicit aTpe: ResultType[A]): ResultType[GetNullable[A]] =
    new ResultType[GetNullable[A]] {
      override type R = Option[aTpe.R]
    }

  implicit def nTpe[A](implicit aTpe: ResultType[A]): ResultType[GetN[A]] =
    new ResultType[GetN[A]] {
      override type R = aTpe.R
    }

  implicit def keyTpe[A](implicit aTpe: ResultType[A]): ResultType[GetKey[A]] =
    new ResultType[GetKey[A]] {
      override type R = aTpe.R
    }

  implicit def fixType(implicit opsTree: OpsTree): ResultType[OpsTree] = {
    val meh = opsTree.unFix match {
      case GetNum            => numTpe
      case GetString         => strTpe
      case GetBool           => boolTpe
      case GetNullable(ops)  => fixType(ops)
      case GetN(n, next)     => fixType(next)
      case GetKey(key, next) => fixType(next)
    }

    new ResultType[OpsTree] {
      override type R = meh.R
    }
  }
}
