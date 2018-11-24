package basil.data
import cats.free.FreeApplicative

object ParseOpsConstructor {
  type ParseTree[I] = HFix[ParseOps, I]

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

    def getAll[F[_], T](alls: FreeApplicative[ParseOps[ParseTree, ?], T]): ExprEnd[ParseOps, T] = {
      ExprEnd(c[T].cont(HFix[ParseOps, T](GetMultiple(alls))))
    }
  }

  val Start: PartialCont[ParseOps] = new PartialCont[ParseOps] {
    override def apply[I]: Continuable[ParseOps, I] = Continuable[ParseOps, I](identity, End)
  }
}

trait PartialCont[A[_[_], _]] {
  def apply[I]: Continuable[A, I]
}

sealed trait ExprTree[A[_[_], _]]
final case class Continuable[A[_[_], _], I](cont: HFix[A, I] => HFix[A, I],
                                            terminator: ExpectedTerminator)
    extends ExprTree[A]
final case class ExprEnd[A[_[_], _], I](t: HFix[A, I]) extends ExprTree[A]
