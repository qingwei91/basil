package basil.parser

import basil.MyFix
import basil.api.OpsTree
import matryoshka.data.Fix
import ParseOps._

trait ParseOpsConstructor {
  def getString: Fix[ParseOps] = Fix[ParseOps](GetString)
  def getN(n: Int, tree: OpsTree): OpsTree = {
    Fix(GetN(n, tree))
  }

  def getNum = Fix[ParseOps](GetNum)
}

object ParseOpsConstructor extends ParseOpsConstructor
