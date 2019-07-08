package basil.derive

import basil.data.ParseFailure
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import org.scalatest.WordSpec

class Testbed extends WordSpec {
  case class Simple(a: Double, b: String, c: Boolean)
  val jsonString1: String =
    """{
      | "a": 200,
      | "b": "hello world!!!!!",
      | "c": false
      |}""".stripMargin



  type In   = (Array[Char], Int)
  type F[A] = Either[ParseFailure, (A, In)]

  import DeriveParseOps._


  def basilRead(): Simple = {
    Parser
      .parseG[In, F, Simple](Start.getType[Simple].eval, jsonString1.toCharArray -> 0)
      .right
      .get
      ._1
  }

  "test" in {
    println(basilRead())
  }

}
