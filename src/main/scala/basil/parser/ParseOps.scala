package basil.parser

import matryoshka.data.Fix
import scalaz._
import syntax.applicative._

sealed trait ParseOps[+A]

case object GetString extends ParseOps[Nothing]
case object GetNum extends ParseOps[Nothing]
case object GetBool extends ParseOps[Nothing]
case class GetNullable[A](ops: A) extends ParseOps[A]
case class GetN[A](n: Int, next: A) extends ParseOps[A]
case class GetKey[A](key: String, next: A) extends ParseOps[A]

trait ResultT[T] {
  type Result
}

object ResultT {
  def apply[A](implicit r: ResultT[A]): ResultT[A] = r

  implicit val getStringR = new ResultT[GetString.type] {
    override type Result = String
  }

  implicit def getNR[A](implicit r: ResultT[A]): ResultT[GetN[A]] =
    new ResultT[GetN[A]] {
      override type Result = r.Result
    }

  implicit def getKeyR[A](implicit r: ResultT[A]): ResultT[GetKey[A]] =
    new ResultT[GetKey[A]] {
      override type Result = r.Result
    }

  implicit def fixR[F[_]](implicit r: ResultT[F[Fix[F]]]): ResultT[Fix[F]] =
    new ResultT[Fix[F]] {
      override type Result = r.Result
    }
}

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
}
