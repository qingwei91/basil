package basil.derive

import basil.data.{GetProduct, GetSum, HFix, ParseOps}
import basil.{FreeParseOps, ParseTree}
import basil.syntax.ParseOpsConstructor._
import cats.data.NonEmptyList
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

    HFix(GetProduct(t))
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = {
    val x: List[Typeclass[T]] = ctx.subtypes.toList.map { subtype =>
      subtype.typeclass.asInstanceOf[Typeclass[T]]
    }

    HFix[ParseOps, T](GetSum(NonEmptyList.fromListUnsafe(x)))
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
