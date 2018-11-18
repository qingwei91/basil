package basil.typeclass

import org.json4s._

trait JsRepr[JVal] {
  type Str <: JVal
  type Num <: JVal
  type Bool <: JVal
  type Null <: JVal

  def str(s: String): Str
  def bool(boolean: Boolean): Bool
  def num(d: Double): Num
  def Null: Null
}

object JsRepr {
  implicit val json4sReps: JsRepr[JValue] = new JsRepr[JValue] {
    override type Str  = JString
    override type Num  = JDouble
    override type Bool = JBool
    override type Null = JNull.type

    override def str(s: String): Str = JString(s)

    override def bool(boolean: Boolean) = JBool(boolean)

    override def num(d: Double) = JDouble(d)

    override def Null: JNull.type = JNull
  }
}
