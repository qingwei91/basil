package basil.parser

case class PError(expect: String, received: Option[String])(implicit l: sourcecode.Line)
  extends Exception(s"Expect [$expect], got $received at line: $l")

object PError {
  def apply(expect: Char, received: Option[Char])(implicit l: sourcecode.Line): PError =
    new PError(expect.toString, received.map(_.toString))

  def apply(expect: Char, received: Char)(implicit l: sourcecode.Line): PError =
    apply(expect, Some(received))

  def apply(expect: String, received: String)(implicit l: sourcecode.Line): PError =
    apply(expect, Some(received))

  def apply(expect: String, received: Char)(implicit l: sourcecode.Line): PError =
    apply(expect, Some(received.toString))
}
