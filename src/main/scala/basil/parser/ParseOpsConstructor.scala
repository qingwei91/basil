package basil.parser

import schemes.Fix

trait ParseOpsConstructor {
  implicit class ContinuableBy(c: Continuable[ParseOps]) {
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

object ParseOpsConstructor extends ParseOpsConstructor
