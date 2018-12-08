package basil.data

import basil.{FreeParseOps, ParseTree}
import cats.free.FreeApplicative.lift

object ParseOpsConstructor {

  /**
    * Syntatic sugar to support creating nested ParseOps structure
    * using builder pattern
    *
    * todo: how much overhead does this adds?
    */
  implicit class PartialContBy(val c: PartialCont[ParseOps]) extends AnyVal {
    def getNum: ExprEnd[ParseOps, Double] = {
      val applied = c[Double]
      ExprEnd(applied.cont(HFix[ParseOps, Double](GetNum(applied.terminator))))
    }
    def getString: ExprEnd[ParseOps, String] = {
      val applied = c[String]
      ExprEnd[ParseOps, String](applied.cont(HFix[ParseOps, String](GetString)))
    }

    def getBool: ExprEnd[ParseOps, Boolean] = {
      val applied = c[Boolean]
      ExprEnd(applied.cont(HFix[ParseOps, Boolean](GetBool)))
    }

    def getKey(key: String): PartialCont[ParseOps] = {
      new PartialCont[ParseOps] {
        override def apply[I]: Continuable[ParseOps, I] = {
          val cont: HFix[ParseOps, I] => HFix[ParseOps, I] = { next =>
            HFix(GetKey(key, next))
          }
          Continuable(cont.andThen(c[I].cont), OneOf(Comma, CurlyBrace))
        }
      }
    }

    def getN(n: Int): PartialCont[ParseOps] = {
      new PartialCont[ParseOps] {
        override def apply[I]: Continuable[ParseOps, I] = {
          val cont: HFix[ParseOps, I] => HFix[ParseOps, I] = { next =>
            HFix(GetN(n, next))
          }
          Continuable(cont.andThen(c[I].cont), OneOf(Comma, Bracket))
        }
      }
    }

    def getAll[I](alls: FreeParseOps[ParseTree, I]): ExprEnd[ParseOps, I] = {
      ExprEnd(c[I].cont(HFix[ParseOps, I](GetMultiple(alls))))
    }
    def getI[I](implicit X: ParseTree[I]): ExprEnd[ParseOps, I] = {
      ExprEnd(c[I].cont(X))
    }
  }

  val Start: PartialCont[ParseOps] = new PartialCont[ParseOps] {
    override def apply[I]: Continuable[ParseOps, I] = Continuable[ParseOps, I](identity, End)
  }

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

trait PartialCont[A[_[_], _]] {
  def apply[I]: Continuable[A, I]
}

final case class Continuable[A[_[_], _], I](cont: HFix[A, I] => HFix[A, I],
                                            terminator: ExpectedTerminator)

final case class ExprEnd[A[_[_], _], I](t: HFix[A, I])
