package basil.parser

import basil.data.{ExpectedTerminator, PPath, ParseOps}
import cats.~>

trait JsonParse[Input, Output[_]] {
  type Parse[I] = Vector[PPath] => Input => Output[I]

  def parseString: Parse[String]
  def parseBoolean: Parse[Boolean]
  def parseNumber(terminator: ExpectedTerminator): Parse[Double]
  def parseArrayItem[I](n: Int, next: Parse[I]): Parse[I]
  def parseObj[I](k: String, nextOp: Parse[I]): Parse[I]

  def parsing: ParseOps[Parse, ?] ~> Parse
}
