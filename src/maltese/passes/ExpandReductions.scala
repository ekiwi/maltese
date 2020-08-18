// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes

import maltese.smt._

/** Expands and/or/xor reductions for potentially better simplifications.
 *  This should ideally be run before Inline because it usually increases the use count.
 *  E.g.: redand(a) => and(a[0], and(a[1], a[2])) // increases the use count from 1 to 3
 * */
object ExpandReductions extends Pass {
  override def name: String = "ExpandReductions"

  override def run(sys: TransitionSystem): TransitionSystem = {
    sys.copy(signals = sys.signals.map(expand))
  }
  private def expand(s: Signal): Signal = s.copy(e = SMTExprVisitor.map(expand)(s.e))
  private def expand(expr: SMTExpr): SMTExpr = expr match {
    case r: BVReduceAnd => Expander.expand(r)
    case r: BVReduceOr => Expander.expand(r)
    case r: BVReduceXor => Expander.expand(r)
    case other => other
  }
}

private object Expander {
  def expand(r: BVReduceAnd): BVExpr = {
    if (r.e.width == 1) { r.e }
    else {
      val allOnes = (BigInt(1) << r.e.width) - 1
      BVEqual(r.e, BVLiteral(allOnes, r.e.width))
    }
  }
  def expand(r: BVReduceOr): BVExpr = {
    if (r.e.width == 1) { r.e }
    else {
      BVNot(BVEqual(r.e, BVLiteral(0, r.e.width)))
    }
  }
  def expand(r: BVReduceXor): BVExpr = {
    if (r.e.width == 1) { r.e }
    else {
      val bits = (0 until r.e.width).map(ii => BVSlice(r.e, ii, ii))
      bits.reduce[BVExpr]((a, b) => BVOp(Op.Xor, a, b))
    }
  }
}
