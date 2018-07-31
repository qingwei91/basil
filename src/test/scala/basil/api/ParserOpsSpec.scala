package basil.api

import org.scalatest.{EitherValues, MustMatchers, WordSpec}
import basil.parser.ParseOpsConstructor._
import basil.parser._
import basil.parser.ParseOps._

class ParserOpsSpec extends WordSpec with MustMatchers with EitherValues {
  "Can derive ResultTpe for fix" in {

    implicit val tree: OpsTree = getN(1, getString)

    ResultType[OpsTree]
  }
}
