// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

import org.scalatest.flatspec.AnyFlatSpec


class SMTSimplifierSpec extends AnyFlatSpec {
  behavior of "SMTSimplifier"

  protected def simplify(e: SMTExpr): SMTExpr = SMTSimplifier.simplify(e)
  private val tru = BVLiteral(1, 1)
  private val fals = BVLiteral(0, 1)
  private val (b, c) = (BVSymbol("b", 1), BVSymbol("c", 1))
  private def and(a: BVExpr, b: BVExpr): BVExpr = BVOp(Op.And, a, b)
  private def or(a: BVExpr, b: BVExpr): BVExpr = BVOp(Op.Or, a, b)
  private def not(a: BVExpr): BVExpr = BVNot(a)

  it should "simplify boolean and" in {

    assert(simplify(and(b, fals)) == fals)
    assert(simplify(and(fals, c)) == fals)

    assert(simplify(and(b, tru)) == b)
    assert(simplify(and(tru, c)) == c)

    assert(simplify(and(b, b)) == b)
    assert(simplify(and(c, c)) == c)

    assert(simplify(and(b, c)) == and(b, c))

    assert(simplify(and(b, not(b))) == fals)
    assert(simplify(and(not(c), c)) == fals)

    assert(simplify(and(not(b), not(b))) == not(b))
  }

  it should "simplify boolean or" in {
    assert(simplify(or(b, fals)) == b)
    assert(simplify(or(fals, c)) == c)

    assert(simplify(or(b, tru)) == tru)
    assert(simplify(or(tru, c)) == tru)

    assert(simplify(or(b, b)) == b)
    assert(simplify(or(c, c)) == c)

    assert(simplify(or(b, c)) == or(b, c))

    assert(simplify(or(b, not(b))) == tru)
    assert(simplify(or(not(c), c)) == tru)

    assert(simplify(or(not(b), not(b))) == not(b))
  }


}
