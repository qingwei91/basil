package basil

import java.util.concurrent.TimeUnit

import basil.derive.DeriveParseOps
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import io.circe.generic.extras.auto._
import io.circe.generic.extras.Configuration
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
  implicit val genDevConfig: Configuration =
    Configuration.default.withDiscriminator("type")

  val jsonString1: String = """{"type":"Z","l":{"type":"X","a":1},"r":{"type":"Y","b":"VVV"}}"""

  import DeriveParseOps._

  @Benchmark
  def basilRead(): ADTBase = {
    Parser.parseString(Start.getType[ADTBase].eval, jsonString1).get
  }

  @Benchmark
  def readCirce(): ADTBase = {
    decode[ADTBase](jsonString1).fold(e => throw e, x => x)
  }

}
