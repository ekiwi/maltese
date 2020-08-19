// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

object SMTExprEval {

  def doBVSlice(e: BigInt, hi: Int, lo: Int): BigInt = (e >> lo) & mask(hi - lo + 1)

  private def mask(width: Int): BigInt = (BigInt(1) << width) - 1
}
