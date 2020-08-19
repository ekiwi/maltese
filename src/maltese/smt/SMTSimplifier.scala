// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

object SMTSimplifier {

  /** Recursively simplifies expressions from bottom to top. */
  def simplify(expr: SMTExpr): SMTExpr = expr.mapExpr(simplify) match {
    case op: BVOp => simplifyOp(op)
    case BVExtend(e, 0, _) => e
    case slice: BVSlice => simplifySlice(slice)
    case BVNot(BVNot(e)) => e
    case ite: BVIte => simplifyBVIte(ite)
    case other => other
  }

  private def simplifyBVIte(i: BVIte): BVExpr = (i.cond, i.tru, i.fals) match {
    // constant condition
    case (BVLiteral(1,1), tru, _) => tru
    case (BVLiteral(0,1), _, fals) => fals

    // same result
    case (_, tru, fals) if tru == fals => tru

    // boolean result (all verified with sympy)
    // simplify_logic(ITE(c, 1, 0)) = c
    case (cond, BVLiteral(1,1), BVLiteral(0,1)) => cond
    // simplify_logic(ITE(c, 0, 1)) = ~c
    case (cond, BVLiteral(0,1), BVLiteral(1,1)) => BVNot(cond)
    // simplify_logic(ITE(c, 1, b)) = b | c
    case (cond, BVLiteral(1,1), b) => or(cond, b)
    // simplify_logic(ITE(c, 0, b)) = b & ~c
    case (cond, BVLiteral(0,1), b) => and(not(cond), b)
    // simplify_logic(ITE(c, b, 1)) = b | ~c
    case (cond, b, BVLiteral(1,1)) => or(not(cond), b)
    // simplify_logic(ITE(c, b, 0)) = b & c
    case (cond, b, BVLiteral(0,1)) => and(cond, b)

    // nested ite
    case (condA, BVIte(condB, truB, falsB), falsA) if falsA == falsB => BVIte(and(condA, condB), truB, falsA)
    case _ => i
  }

  private def simplifyOp(expr: BVOp): BVExpr = expr match {
    case BVOp(Op.And, a, BVLiteral(1, 1)) => a
    case BVOp(Op.And, BVLiteral(1, 1), b) => b
    case BVOp(Op.And, _, BVLiteral(0, 1)) => BVLiteral(0, 1)
    case BVOp(Op.And, BVLiteral(0, 1), _) => BVLiteral(0, 1)
    case BVOp(Op.Or, _, BVLiteral(1, 1)) => BVLiteral(1, 1)
    case BVOp(Op.Or, BVLiteral(1, 1), _) => BVLiteral(1, 1)
    case BVOp(Op.Or, a, BVLiteral(0, 1)) => a
    case BVOp(Op.Or, BVLiteral(0, 1), b) => b
    case other => other
  }

  // we try to "push" slice expressions as far down as possible
  // e.g. concat(1'b1, 1'b0)[0] => 1'b0
  private def simplifySlice(expr: BVSlice): BVExpr = expr match {
    // no-op
    case BVSlice(e, hi, 0) if hi == e.width - 1 => e
    case other => other
  }

  private def and(a: BVExpr, b: BVExpr): BVOp = BVOp(Op.And, a, b)
  private def or(a: BVExpr, b: BVExpr): BVOp = BVOp(Op.Or, a, b)
  private def not(a: BVExpr): BVNot = BVNot(a)
}
