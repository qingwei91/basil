package basil.magnolia

import basil.data.ParseOpsConstructor._
import basil.data.{GetMultiple, HFix}
import basil.{FreeParseOps, ParseTree}
import cats.instances.list._
import cats.syntax.traverse._
import magnolia.{CaseClass, Magnolia, SealedTrait}

object DeriveParseOps {
  type Typeclass[T] = ParseTree[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    val t = caseClass.parameters.toList
      .traverse[FreeParseOps[Typeclass, ?], Any] { param =>
        getKeyFree(param.label, param.typeclass).map[Any](identity)
      }
      .map(p => caseClass.rawConstruct(p))

    HFix(GetMultiple(t))
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
