// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

object SMTExprEval {

  def doBVSlice(e: BigInt, hi: Int, lo: Int): BigInt = {
    val width = hi - lo + 1
    val mask = (BigInt(1) << width) - 1
    (e >> lo) & mask
  }
}
