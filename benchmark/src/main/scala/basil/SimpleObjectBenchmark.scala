package basil

import java.util.concurrent.TimeUnit

import basil.data.ParseFailure
import basil.derive.DeriveParseOps
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._

import org.openjdk.jmh.annotations._

case class Simple(a: Double, b: String, c: Boolean)

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class SimpleObjectBenchmark {

  val jsonString1: String =
    """{
      | "a": 200,
      | "b": "hello\t world!!!!!",
      | "c": false,
      | "e": false,
      | "f": false,
      | "g": false,
      | "h": false,
      | "meh": {
      |   "halo": "world",
      |   "ni": "world",
      |   "hao": "world",
      |   "hehe": "world",
      |   "my": "world"
      | },
      | "i": false,
      | "j": false,
      | "k": false,
      | "l": false
      |}""".stripMargin

  type In   = (Array[Char], Int)
  type F[A] = Either[ParseFailure, A]

  import DeriveParseOps._
  val query = Start.getType[Simple].eval

  @Benchmark
  def basilRead(): Simple = {
    Parser
      .parseString[F, Simple](query, jsonString1)
      .right
      .get
  }

  import io.circe.generic.extras.auto._
  import io.circe.generic.extras.Configuration
  import io.circe.parser._
  implicit val genDevConfig: Configuration =
    Configuration.default.withDiscriminator("type")

  @Benchmark
  def readCirce(): Simple = {
    decode[Simple](jsonString1).fold(e => throw e, x => x)
  }

  import upickle.default._
  @Benchmark
  def readUPickle(): Simple = {
    read[Simple](jsonString1)(macroR[Simple])
  }
}
