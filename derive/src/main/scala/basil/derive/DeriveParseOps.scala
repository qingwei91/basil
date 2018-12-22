package basil.derive

import basil.ParseTree
import basil.data._
import basil.typeclass.Lazy
import cats.data.NonEmptyMap
import cats.instances.string._
import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import cats.instances.list._
import cats.syntax.traverse._
import magnolia.{CaseClass, Magnolia, SealedTrait}

import scala.collection.immutable.SortedMap

object DeriveParseOps {
  type Typeclass[T] = ParseTree[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {

    val t = caseClass.parameters.toList
      .traverse[FreeApplicative[ParseTree, ?], Any] { param =>
        val next = param.typeclass.asInstanceOf[Typeclass[Any]]

        lift(HFix(GetKey(param.label, next)))
      }
      .map(p => caseClass.rawConstruct(p))

    HFix(GetProduct(t))
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = {
    val possibleTypeclasses = ctx.subtypes.toList.map { subtype =>
      subtype.typeName.short ->
        Lazy(
          subtype.typeclass.asInstanceOf[Typeclass[T]]
        )
    }

    val sorted = SortedMap
      .empty[String, Lazy[Typeclass[T]]] ++ possibleTypeclasses

    HFix[ParseOps, T](GetSum(NonEmptyMap.fromMapUnsafe(sorted)))
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
