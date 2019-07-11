package basil

import java.util.concurrent.TimeUnit

import basil.data.ParseFailure
import basil.derive.DeriveParseOps
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.parser._
import org.openjdk.jmh.annotations._

import scala.util.Try

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

  type In   = (Array[Char], Int)
  type F[A] = Either[ParseFailure, A]

  import DeriveParseOps._

  @Benchmark
  def basilRead(): ADTBase = {
    Parser
      .parseG[In, F, ADTBase](Start.getType[ADTBase].eval, jsonString1.toCharArray -> 0)
      .right
      .get
  }

  @Benchmark
  def basilStream(): ADTBase = {
    Parser
      .parseSource[Lambda[A => Try[List[A]]], ADTBase](Start.getType[ADTBase].eval,
                                                       Try(jsonString1.toCharArray.toList))
      .get
      .head
      ._1
  }

  @Benchmark
  def readCirce(): ADTBase = {
    decode[ADTBase](jsonString1).fold(e => throw e, x => x)
  }

}
