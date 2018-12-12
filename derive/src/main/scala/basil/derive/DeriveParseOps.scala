package basil.derive

import basil.data.{GetMultiple, HFix}
import basil.{FreeParseOps, ParseTree}
import basil.syntax.ParseOpsConstructor._
import cats.instances.list._
import cats.syntax.traverse._
import magnolia.{CaseClass, Magnolia}

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

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
