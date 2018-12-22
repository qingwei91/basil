package basil

import java.util.concurrent.TimeUnit

import basil.derive.DeriveParseOps
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import io.circe.generic.auto._
import io.circe.parser._
import org.openjdk.jmh.annotations._

sealed trait ADTBase                 extends Product with Serializable
case class X(a: Double)              extends ADTBase
case class Y(b: String)              extends ADTBase
case class Z(l: ADTBase, r: ADTBase) extends ADTBase

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class ADTBenchmark {
  val obj: ADTBase        = Z(X(1), Y("VVV"))
  val jsonString1: String = """{"type":"Z","l":{"type":"X","a":1},"r":{"type":"Y","b":"VVV"}}"""
  val jsonString2: String = """{"l":{"a":1,"type":"X"},"r":{"b":"VVV","type":"Y"},"type":"Z"}"""
  val jsonString3: String = """{"r":{"b":"VVV","type":"Y"},"type":"Z","l":{"a":1,"type":"X"}}"""

  import DeriveParseOps._

  @Benchmark
  def basilRead(): ADTBase = {
    Parser.parseString(Start.getType[ADTBase].eval, jsonString1).get
  }

  @Benchmark
  def readCirce(): ADTBase = decode[ADTBase](jsonString1).fold(throw _, x => x)

}
