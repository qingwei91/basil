package basil.parser

import cats.Show

sealed trait PPath

/**
  * questionable design
  * Designed this way so we can mark a path as nullable without knowing
  * what the path actually is
  */
case object Nullable extends PPath

final case class KeyPath(key: String) extends PPath
final case class IndexPath(i: Int)    extends PPath

object PPath {
  import cats.syntax.show._
  import cats.instances.vector._
  import cats.instances.string._
  import cats.syntax.foldable._

  implicit def pathsShow: Show[Vector[PPath]] = (t: Vector[PPath]) => {
    t.foldMap(_.show)
  }

  implicit val pathShow: Show[PPath] = {
    case KeyPath(key) => s"""["$key"]"""
    case IndexPath(i) => s"[$i]"
    case Nullable => "?"
  }

  implicit class PathOps(p: Vector[PPath]) {
    def \(k: String): Vector[PPath] = p :+ KeyPath(k)
    def \(i: Int): Vector[PPath]    = p :+ IndexPath(i)
    def ? : Vector[PPath]           = p :+ Nullable
  }
}
