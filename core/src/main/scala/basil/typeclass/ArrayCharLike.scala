package basil.typeclass

trait ArrayCharLike[T] {
  def apply(t: T, idx: Int): Char
  def isDefinedAt(t: T, idx: Int): Boolean
  def slice(t: T, from: Int, until: Int): T
  def sameElements(a: T, b: T): Boolean
  def formString(t: T): String
  def length(t: T): Int
}

object ArrayCharLike {
  implicit class Ops[T](t: T)(implicit T: ArrayCharLike[T]) {
    def apply(idx: Int): Char           = T.apply(t, idx)
    def isDefinedAt(idx: Int): Boolean  = T.isDefinedAt(t, idx)
    def slice(from: Int, until: Int): T = T.slice(t, from, until)
    def sameElements(that: T): Boolean  = T.sameElements(t, that)
    def formString: String              = T.formString(t)
    def length: Int = T.length(t)
  }
}
