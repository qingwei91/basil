package basil.parser

import cats.syntax.show._

final case class ParseFailure(msg: String, path: Vector[PPath] = Vector.empty)(
    implicit l: sourcecode.Line)
    extends Exception {
  override def getMessage: String =
    s"""$msg at line: $l when parsing ${path.show}"""
}

object ParseFailure {

  def apply(expect: String, received: String, path: Vector[PPath])(
      implicit l: sourcecode.Line): ParseFailure =
    apply(s"Expect $expect, but got $received", path)

  def termination(implicit path: Vector[PPath], l: sourcecode.Line): ParseFailure =
    ParseFailure("Unexpected termination", path)
}
