package basil.typeclass

trait Cons[Col[_]] {
  def cons[E](cols: Col[E], e: E): Col[E]
}
