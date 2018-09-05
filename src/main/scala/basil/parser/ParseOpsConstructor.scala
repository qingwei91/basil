package basil.parser

import matryoshka.data.Fix

trait ParseOpsConstructor {
  implicit class ContinuableBy(c: Continuable[ParseOps]) {
    def getNum: End[ParseOps] = End(c.cont(Fix[ParseOps](GetNum)))

    def getN(n: Int): Continuable[ParseOps] = {
      val cont: Fix[ParseOps] => Fix[ParseOps] = { next =>
        Fix(GetN(n, next))
      }

      Continuable(cont.andThen(c.cont))
    }

    def getString: End[ParseOps] = End(c.cont(Fix[ParseOps](GetString)))
  }

  val Start: Continuable[ParseOps] = Continuable[ParseOps](identity)
}

sealed trait ExprTree[A[_]]
case class Continuable[A[_]](cont: Fix[A] => Fix[A]) extends ExprTree[A]
case class End[A[_]](t: Fix[A])                      extends ExprTree[A]

object ParseOpsConstructor extends ParseOpsConstructor
