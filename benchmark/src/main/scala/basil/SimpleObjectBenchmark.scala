package basil

import java.util.concurrent.TimeUnit

import basil.data.ParseFailure
import basil.derive.DeriveParseOps
import basil.parser.Parser
import basil.parser.implicits._
import basil.syntax.ParseOpsConstructor._
import io.circe.generic.extras.auto._
import io.circe.generic.extras.Configuration
import io.circe.parser._
import org.openjdk.jmh.annotations._

case class Simple(a: Double, b: String, c: Boolean)

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class SimpleObjectBenchmark {
  implicit val genDevConfig: Configuration =
    Configuration.default.withDiscriminator("type")

  val jsonString1: String =
    """{
      | "a": 200,
      | "b": "hello\t world!!!!!",
      | "c": false
      |}""".stripMargin

  type In   = (Array[Char], Int)
  type F[A] = Either[ParseFailure, A]

  import DeriveParseOps._

  @Benchmark
  def basilRead(): Simple = {
    Parser
      .parseG[In, F, Simple](Start.getType[Simple].eval, jsonString1.toCharArray -> 0)
      .right
      .get
  }

  @Benchmark
  def readCirce(): Simple = {
    decode[Simple](jsonString1).fold(e => throw e, x => x)
  }

}
