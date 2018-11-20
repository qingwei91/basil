package basil.data

object ParseOpsConstructor {

  /**
    * Syntatic sugar to support creating nested ParseOps structure
    * using builder pattern
    *
    * todo: how much overhead does this adds?
    */
  implicit class ContinuableBy[I](val c: Continuable[ParseOps, I]) extends AnyVal {

    def getNum(implicit eq: I =:= Double): ExprEnd[ParseOps, I] = {
      val _ = eq
      ExprEnd(c.cont(HFix[ParseOps, Double](GetNum(c.terminator)).asInstanceOf[HFix[ParseOps, I]]))
    }

    def getString(implicit eq: I =:= String): ExprEnd[ParseOps, I] = {
      val _ = eq
      ExprEnd[ParseOps, I](
        c.cont(HFix[ParseOps, String](GetString).asInstanceOf[HFix[ParseOps, I]]))
    }

    def getBool(implicit eq: I =:= Boolean): ExprEnd[ParseOps, I] = {
      val _ = eq
      ExprEnd(c.cont(HFix[ParseOps, Boolean](GetBool).asInstanceOf[HFix[ParseOps, I]]))
    }

    def getKey(key: String): Continuable[ParseOps, I] = {
      val cont: HFix[ParseOps, I] => HFix[ParseOps, I] = { next =>
        HFix(GetKey(key, next))
      }

      Continuable(cont.andThen(c.cont), OneOf(Comma, CurlyBrace))
    }

    def getN(n: Int): Continuable[ParseOps, I] = {
      val cont: HFix[ParseOps, I] => HFix[ParseOps, I] = { next =>
        HFix(GetN(n, next))
      }

      Continuable(cont.andThen(c.cont), OneOf(Comma, Bracket))
    }

//    todo: how to handle Nullable ?
//    def getNullable: Continuable[ParseOps, I] = {
//      val cont: HFix[ParseOps, I] => HFix[ParseOps, I] = { next =>
//        HFix(GetNullable(next))
//      }
//
//      Continuable(cont.andThen(c.cont), c.terminator)
//    }
  }

  def Start[I]: Continuable[ParseOps, I] = Continuable[ParseOps, I](identity, End)
}

sealed trait ExprTree[A[_[_], _]]
final case class Continuable[A[_[_], _], I](cont: HFix[A, I] => HFix[A, I],
                                            terminator: ExpectedTerminator)
    extends ExprTree[A]
final case class ExprEnd[A[_[_], _], I](t: HFix[A, I]) extends ExprTree[A]
