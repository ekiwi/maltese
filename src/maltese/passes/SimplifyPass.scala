// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes

import maltese.smt._

/** simplifies signals where possible  */
object SimplifyPass {

  def run(sys: TransitionSystem): TransitionSystem = {
    sys.copy(signals = sys.signals.map(simplify))
  }

  private def simplify(s: Signal): Signal = s.copy(e = SMTExprVisitor.map(simplify)(s.e))

  private def simplify(e: SMTExpr): SMTExpr = e match {
    // TODO: maybe generalize by changing BVReduceAnd(x) => x == 1111 => split up concat
    case BVReduceAnd(BVConcat(a, b)) if a.width == 1 && b.width == 1 => BVOp(Op.And, a, b)
    case BVReduceOr(BVConcat(a, b)) if a.width == 1 && b.width == 1 => BVOp(Op.Or, a, b)
    case BVExtend(e, 0, _) => e
    case BVSlice(e, hi, 0) if hi == e.width - 1 => e
    case other => other
  }
}
