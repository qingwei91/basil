package basil.parser

sealed trait CheckPoint
case object Root         extends CheckPoint
case object ArrayComma   extends CheckPoint
case object ObjComma     extends CheckPoint
case object Colon        extends CheckPoint
case object OpenQuote    extends CheckPoint
case object CloseQuote   extends CheckPoint
case object CurlyBracket extends CheckPoint
case object Bracket      extends CheckPoint
