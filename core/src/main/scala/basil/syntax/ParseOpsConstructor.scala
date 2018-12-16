package basil.syntax

import basil.data._
import basil.{FreeParseOps, ParseTree}
import cats.Id
import cats.free.FreeApplicative.lift

object ParseOpsConstructor extends FreeParseOpsInstances {

  /**
    * Syntatic sugar to support creating nested ParseOps structure
    * using builder pattern
    */
  implicit class PartialCont[F[_]](val outer: PartiallyAppliedCont[ParseOps, F]) extends AnyVal {

    def getNum: ExprEnd[ParseOps, F[Double]] = {
      val applied = outer[Double]
      ExprEnd(applied.cont(HFix[ParseOps, Double](GetNum(applied.terminator))))
    }

    def getString: ExprEnd[ParseOps, F[String]] = {
      val applied = outer[String]
      ExprEnd[ParseOps, F[String]](applied.cont(HFix[ParseOps, String](GetString)))
    }

    def getBool: ExprEnd[ParseOps, F[Boolean]] = {
      val applied = outer[Boolean]
      ExprEnd(applied.cont(HFix[ParseOps, Boolean](GetBool)))
    }

    def getType[I](implicit X: ParseTree[I]): ExprEnd[ParseOps, F[I]] = {
      ExprEnd(outer[I].cont(X))
    }

    def getAll[I](alls: FreeParseOps[ParseTree, I]): ExprEnd[ParseOps, F[I]] = {
      ExprEnd(outer[I].cont(HFix[ParseOps, I](GetProduct(alls))))
    }

    def getKey(key: String): PartiallyAppliedCont[ParseOps, F] = {
      new PartiallyAppliedCont[ParseOps, F] {
        override def apply[I]: Cont[ParseOps, F, I] = {
          val cont: HFix[ParseOps, I] => HFix[ParseOps, F[I]] = { next =>
            val inner = HFix(GetKey(key, next))
            outer[I].cont(inner)
          }

          Cont(cont, OneOf(Comma, CurlyBrace))
        }
      }
    }

    def getN(n: Int): PartiallyAppliedCont[ParseOps, F] = {
      new PartiallyAppliedCont[ParseOps, F] {
        override def apply[I]: Cont[ParseOps, F, I] = {
          val cont: HFix[ParseOps, I] => HFix[ParseOps, F[I]] = { next =>
            val inner = HFix(GetN(n, next))
            outer[I].cont(inner)
          }
          Cont(cont, OneOf(Comma, Bracket))
        }
      }
    }

    type FO[X] = F[Option[X]]
    def getOpt: PartiallyAppliedCont[ParseOps, FO] = {
      new PartiallyAppliedCont[ParseOps, FO] {
        override def apply[I]: Cont[ParseOps, FO, I] = {
          val cont: HFix[ParseOps, I] => HFix[ParseOps, F[Option[I]]] = { next =>
            val inner = HFix(GetOpt(next): ParseOps[HFix[ParseOps, ?], Option[I]])

            outer[Option[I]].cont(inner)
          }

          Cont[ParseOps, FO, I](cont, OneOf(Comma, CurlyBrace, Bracket))
        }
      }
    }

  }

  val Start: PartiallyAppliedCont[ParseOps, Id] = new PartiallyAppliedCont[ParseOps, Id] {
    override def apply[I]: Cont[ParseOps, Id, I] = Cont[ParseOps, Id, I](identity, End)
  }

}

trait FreeParseOpsInstances {
  implicit val getString: HFix[ParseOps, String] = HFix[ParseOps, String](GetString)
  implicit val getBool: HFix[ParseOps, Boolean]  = HFix[ParseOps, Boolean](GetBool)

  /** todo: Exposing this via implicit looks dangerous, consider separate scope
    * Right now it is used for automatic case class derivation, which is
    * encoded as JSON object and thus we can expect the terminator to always be
    * OneOf(Comma, CurlyBrace)
    * */
  implicit val getNum: HFix[ParseOps, Double] =
    HFix[ParseOps, Double](GetNum(OneOf(Comma, CurlyBrace)))

  implicit def getOpt[I](implicit getI: HFix[ParseOps, I]): HFix[ParseOps, Option[I]] = {
    HFix[ParseOps, Option[I]](GetOpt(getI))
  }

  implicit def getNumFree[F[_]]: FreeParseOps[F, Double] =
    lift[ParseOps[F, ?], Double](GetNum(End))

  implicit def getStringFree[F[_]]: FreeParseOps[F, String] =
    lift[ParseOps[F, ?], String](GetString)

  implicit def getBoolFree[F[_]]: FreeParseOps[F, Boolean] =
    lift[ParseOps[F, ?], Boolean](GetBool)

  def getKeyFree[F[_], I](key: String, next: F[I]): FreeParseOps[F, I] =
    lift[ParseOps[F, ?], I](GetKey(key, next))
}

trait PartiallyAppliedCont[A[_[_], _], F[_]] {
  def apply[I]: Cont[A, F, I]
}

final case class Cont[A[_[_], _], F[_], I](cont: HFix[A, I] => HFix[A, F[I]],
                                           terminator: ExpectedTerminator)

final case class ExprEnd[A[_[_], _], I](eval: HFix[A, I])
