package basil.parser

import cats.syntax.show._

case class PError(msg: String, path: Vector[PPath])(implicit l: sourcecode.Line) extends Exception {
  override def getMessage: String =
    s"""$msg at line: $l when parsing ${path.show}"""
}

object PError {

  def apply(expect: String, received: String, path: Vector[PPath])(
      implicit l: sourcecode.Line): PError =
    apply(s"Expect $expect, but got $received", path)

  def termination(implicit path: Vector[PPath], l: sourcecode.Line): PError =
    PError("Unexpected termination", path)
}
