package basil.api

import basil.parser.{GetN, GetString, ParseOps}
import basil.parser.ParseOps._
import matryoshka.data.Fix
import org.json4s.JsonAST.JString
import org.scalatest._
import basil.parser.ParseOpsConstructor._

class ParserSpec extends WordSpec with MustMatchers with EitherValues {
  "Parser" should {
    "parse String" in {
      val tree = getString

      val fn = Parser.parse(tree)

      val jsonStr = "\"Halo\"".toCharArray
      val parseState = ParsingState(jsonStr, 0, RootC)
      fn(parseState).right.get mustBe Parsed(5, JString("Halo"))
    }

    "parse array" in {

      val tree: OpsTree = getN(1, getString)

      val fn = Parser.parse(tree)
      val jsonStr = """["oo", "hoho", "meh"]""".toCharArray

      val parseState = ParsingState(jsonStr, 0, RootC)
      fn(parseState).right.get.value mustBe JString("hoho")
    }

    "parse nested array" in {
      val tree = getN(1, getN(2, getString))
      val fn = Parser.parse(tree)

      val jsonStr = """["oo", ["hoho", "meh", "qooo"], "meh"]""".toCharArray

      val parseState = ParsingState(jsonStr, 0, RootC)

      fn(parseState).right.get.value mustBe JString("qooo")
    }
  }
}
