package basil.typeclass

/**
  * Typeclass to provide capability of prepending an element into
  * a source of element
  *
  * todo: Create laws for it with drop?
  */
trait Cons[Col[_]] {
  def cons[E](cols: Col[E], e: E): Col[E]
}
