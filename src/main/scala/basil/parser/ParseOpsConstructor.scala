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
}

object ParseOpsConstructor extends ParseOpsConstructor
