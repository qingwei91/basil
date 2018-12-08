package basil.data

import cats.syntax.show._

final case class ParseFailure(msg: String, path: Vector[PPath] = Vector.empty) extends Exception {
  override def getMessage: String =
    s"""$msg when parsing ${path.show}"""
}

object ParseFailure {

  def apply(expect: String, received: String, path: Vector[PPath]): ParseFailure =
    ParseFailure(s"Expect $expect, but got $received", path)

  def termination(implicit path: Vector[PPath]): ParseFailure =
    ParseFailure("Unexpected termination", path)
}

sealed trait ParseError
case class KeyMissingError(key: String, path: Vector[PPath]) extends ParseError
case class UnexpectedType(expectedType: String, unexpectedChar: Char, path: Vector[PPath])
    extends ParseError
