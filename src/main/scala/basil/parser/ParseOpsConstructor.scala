package basil.parser

import basil.api.OpsTree
import matryoshka.data.Fix

trait ParseOpsConstructor {
  def getString: OpsTree = Fix[ParseOps](GetString)
  def getN(n: Int, tree: OpsTree): OpsTree = {
    Fix(GetN(n, tree))
  }
}

object ParseOpsConstructor extends ParseOpsConstructor
