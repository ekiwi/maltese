// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

object SMTSimplifier {

  /** Recursively simplifies expressions from bottom to top. */
  def simplify(expr: SMTExpr): SMTExpr = expr.mapExpr(simplify) match {
    case op: BVOp => simplifyOp(op)
    case eq: BVEqual => simplifyBVEqual(eq)
    case BVExtend(e, 0, _) => e
    case slice: BVSlice => simplifySlice(slice)
    case BVNot(BVNot(e)) => e
    case ite: BVIte => simplifyBVIte(ite)
    case other => other
  }

  private def simplifyBVEqual(expr: BVEqual): BVExpr = (expr.a, expr.b) match {
    case (a, b) if a == b => True()
    case (a, True()) => a
    case (True(), a) => a
    case (a, False()) => not(a)
    case (False(), a) => not(a)
    case (_, _) => expr
  }

  private def simplifyBVIte(i: BVIte): BVExpr = (i.cond, i.tru, i.fals) match {
    // constant condition
    case (True(), tru, _) => tru
    case (False(), _, fals) => fals

    // same result
    case (_, tru, fals) if tru == fals => tru

    // boolean result (all verified with sympy)
    // simplify_logic(ITE(c, 1, 0)) = c
    case (cond, True(), False()) => cond
    // simplify_logic(ITE(c, 0, 1)) = ~c
    case (cond, False(), True()) => BVNot(cond)
    // simplify_logic(ITE(c, 1, b)) = b | c
    case (cond, True(), b) => or(cond, b)
    // simplify_logic(ITE(c, 0, b)) = b & ~c
    case (cond, False(), b) => and(not(cond), b)
    // simplify_logic(ITE(c, b, 1)) = b | ~c
    case (cond, b, True()) => or(not(cond), b)
    // simplify_logic(ITE(c, b, 0)) = b & c
    case (cond, b, False()) => and(cond, b)

    // nested ite
    case (condA, BVIte(condB, truB, falsB), falsA) if falsA == falsB => BVIte(and(condA, condB), truB, falsA)
    case _ => i
  }

  private def simplifyOp(expr: BVOp): BVExpr = expr match {
    case BVOp(Op.And, a, True()) => a
    case BVOp(Op.And, True(), b) => b
    case BVOp(Op.And, _, False()) => BVLiteral(0, 1)
    case BVOp(Op.And, False(), _) => BVLiteral(0, 1)
    case BVOp(Op.Or, _, True()) => BVLiteral(1, 1)
    case BVOp(Op.Or, True(), _) => BVLiteral(1, 1)
    case BVOp(Op.Or, a, False()) => a
    case BVOp(Op.Or, False(), b) => b
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

  // unapply for matching BVLiteral(1, 1)
  private object True  {
    private val _True = BVLiteral(1, 1)
    def apply(): BVLiteral = _True
    def unapply(l: BVLiteral): Boolean = l.value == 1 && l.width == 1
  }
  // unapply for matching BVLiteral(0, 1)
  private object False {
    private val _False = BVLiteral(0, 1)
    def apply(): BVLiteral = _False
    def unapply(l: BVLiteral): Boolean = l.value == 0 && l.width == 1
  }
}
