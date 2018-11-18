package basil.data

import schemes.Fix

object ParseOpsConstructor {

  /**
    * Syntatic sugar to support creating nested ParseOps structure
    * using builder pattern
    *
    * todo: how much overhead does this adds?
    */
  implicit class ContinuableBy(val c: Continuable[ParseOps]) extends AnyVal {
    def getKey(key: String): Continuable[ParseOps] = {
      val cont: Fix[ParseOps] => Fix[ParseOps] = { next =>
        Fix(GetKey(key, next))
      }

      Continuable(cont.andThen(c.cont), OneOf(Comma, CurlyBrace))

    }
    def getNum: ExprEnd[ParseOps] = {
      ExprEnd(c.cont(Fix[ParseOps](GetNum(c.terminator))))
    }

    def getN(n: Int): Continuable[ParseOps] = {
      val cont: Fix[ParseOps] => Fix[ParseOps] = { next =>
        Fix(GetN(n, next))
      }

      Continuable(cont.andThen(c.cont), OneOf(Comma, Bracket))
    }

    def getString: ExprEnd[ParseOps] = ExprEnd(c.cont(Fix[ParseOps](GetString)))

    def getBool: ExprEnd[ParseOps] = ExprEnd(c.cont(Fix[ParseOps](GetBool)))

    def getNullable: Continuable[ParseOps] = {
      val cont: Fix[ParseOps] => Fix[ParseOps] = { next =>
        Fix(GetNullable(next))
      }

      Continuable(cont.andThen(c.cont), c.terminator)
    }
  }

  val Start: Continuable[ParseOps] = Continuable[ParseOps](identity, End)
}

sealed trait ExprTree[A[_]]
case class Continuable[A[_]](cont: Fix[A] => Fix[A], terminator: ExpectedTerminator)
    extends ExprTree[A]
case class ExprEnd[A[_]](t: Fix[A]) extends ExprTree[A]
