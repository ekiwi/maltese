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

  private def simplify(expr: SMTExpr): SMTExpr = expr match {
    case e: BVOp => simplifyOp(e)
    // TODO: maybe generalize by changing BVReduceAnd(x) => x == 1111 => split up concat
    case BVReduceAnd(BVConcat(a, b)) if a.width == 1 && b.width == 1 => BVOp(Op.And, a, b)
    case BVReduceOr(BVConcat(a, b)) if a.width == 1 && b.width == 1 => BVOp(Op.Or, a, b)
    case BVExtend(e, 0, _) => e
    case BVSlice(e, hi, 0) if hi == e.width - 1 => e
    case BVNot(BVNot(e)) => e
    case BVIte(condA, BVIte(condB, truB, falsB), falsA) if falsA == falsB => BVIte(and(condA, condB), truB, falsA)
    case other => other
  }

  private def simplifyOp(expr: BVOp): BVExpr = expr match {
      // TODO
//    case BVOp(Op.And, a, BVLiteral(1, 1)) => a
//    case BVOp(Op.And, BVLiteral(1, 1), b) => b
//    case BVOp(Op.And, _, BVLiteral(0, 1)) => BVLiteral(0, 1)
//    case BVOp(Op.And, BVLiteral(0, 1), _) => BVLiteral(0, 1)
//    case BVOp(Op.Or, _, BVLiteral(1, 1)) => BVLiteral(1, 1)
//    case BVOp(Op.Or, BVLiteral(1, 1), _) => BVLiteral(1, 1)
//    case BVOp(Op.Or, a, BVLiteral(0, 1)) => a
//    case BVOp(Op.Or, BVLiteral(0, 1), b) => b
    case other => other
  }

  private def and(a: BVExpr, b: BVExpr): BVExpr = BVOp(Op.And, a, b)
}
