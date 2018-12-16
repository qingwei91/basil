package basil

import basil.derive.DeriveParseOps
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import io.circe.parser._
import io.circe.syntax._
import org.openjdk.jmh.annotations.Benchmark

sealed trait ADTBase                 extends Product with Serializable
case class X(a: Int)                 extends ADTBase
case class Y(b: String)              extends ADTBase
case class Z(l: ADTBase, r: ADTBase) extends ADTBase

class ADTBenchmark {
  var obj: ADTBase        = Z(X(1), Y("VVV"))
  var jsonString1: String = """{"type":"Z","l":{"type":"X","a":1},"r":{"type":"Y","b":"VVV"}}"""
  var jsonString2: String = """{"l":{"a":1,"type":"X"},"r":{"b":"VVV","type":"Y"},"type":"Z"}"""
  var jsonString3: String = """{"r":{"b":"VVV","type":"Y"},"type":"Z","l":{"a":1,"type":"X"}}"""

  import DeriveParseOps._

  @Benchmark
  def basilRead() = {
    Parser.parseString(Start.getType[Z].eval, jsonString1).get
  }

  @Benchmark
  def readCirce(): ADTBase = decode[ADTBase](jsonString1).fold(throw _, x => x)

}
