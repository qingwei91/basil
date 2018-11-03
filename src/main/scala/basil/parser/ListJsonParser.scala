package basil.parser

import basil.typeclass.instances._
import cats.instances.list._
import org.json4s.JsonAST.JValue

object ListJsonParser extends JsonParse[TryList, JValue]
