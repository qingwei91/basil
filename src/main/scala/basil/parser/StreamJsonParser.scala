package basil.parser

import basil.typeclass.instances._
import cats.effect.IO
import fs2.Stream
import org.json4s.JsonAST._

object StreamJsonParser extends JsonParse[Stream[IO, ?], JValue] {
  override type Str  = JString
  override type Num  = JDouble
  override type Bool = JBool
  override type Arr  = JArray
  override type Obj  = JObject
  override type Null = JNull.type

  override def str(s: String): Str = JString(s)

  override def bool(boolean: Boolean) = JBool(boolean)

  override def num(d: Double) = JDouble(d)

  override def Null: JNull.type = JNull
}
