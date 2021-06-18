// Copyright 2019-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.sym

import maltese.smt._
import org.scalatest.flatspec.AnyFlatSpec

class BVValueSummaryTests extends AnyFlatSpec {
  implicit val ctx = new SymbolicContext(Options.Default)

  private def bv4(value: Int) = BVValueSummary(value, 4)
  private def add_sym(a: BVExpr, b: BVExpr) = BVOp(Op.Add, a, b)

  it should "pruning of infeasible entries" in {
    // for this test we create a `false` guard in the value summary
    val a = BVValueSummary(BVSymbol("a", 1))
    val four_or_five = BVValueSummary.ite(a, bv4(4), bv4(5))
    val one_or_one = BVValueSummary.ite(a, bv4(1), bv4(1))

    // if we add the two value summaries, we should get no more than two entries,
    // since the other entries should evaluate to false
    val sum = BVValueSummary.binary(four_or_five, one_or_one, add_sym)
    assert(sum.size == 2)
  }

  it should "coalescing of duplicate values" in {
    val a = BVValueSummary(BVSymbol("a", 1))
    val four_or_five = BVValueSummary.ite(a, bv4(4), bv4(5))
    val one_or_zero = BVValueSummary.ite(a, bv4(1), bv4(0))

    // the result of the addition is always five, no matter what the branch condition is
    val sum = BVValueSummary.binary(four_or_five, one_or_zero, add_sym)
    assert(sum.size == 1)
    assert(sum.isConcrete)
    assert(sum.value.get == 5)
  }

  it should "concrete ite" in {
    val tru = BVValueSummary(true)
    val fals = BVValueSummary(false)
    val four = BVValueSummary(4, 4)
    val five = BVValueSummary(5, 4)

    assert(BVValueSummary.ite(tru,  four, five).isConcrete)
    assert(BVValueSummary.ite(tru,  four, five).value.get == 4)
    assert(BVValueSummary.ite(fals, four, five).isConcrete)
    assert(BVValueSummary.ite(fals, four, five).value.get == 5)
  }

  it should "concrete unop" in {
    val four = BVValueSummary(4, 4)

    def addFive(a: BVExpr): BVExpr = BVOp(Op.Add, a, BVLiteral(5, 4))
    assert(BVValueSummary.unary(four, addFive).isConcrete)
    assert(BVValueSummary.unary(four, addFive).value.get == 9)
    assert(BVValueSummary.unary(four, addFive).width == 4)
  }

  it should "concrete binop" in {
    val four = BVValueSummary(4, 4)
    val five = BVValueSummary(5, 4)

    assert(BVValueSummary.binary(four, five, add_sym).isConcrete)
    assert(BVValueSummary.binary(four, five, add_sym).value.get == 9)
    assert(BVValueSummary.binary(four, five, add_sym).width == 4)
  }

  it should "support and" in {
    val tru = BVValueSummary(true)
    val fals = BVValueSummary(false)

    def and(a: BVValueSummary, b: BVValueSummary): BVValueSummary = {
      BVValueSummary.binary(a, b, BVAnd(_, _))
    }

    assert(and( tru, fals).value.get == 0)
    assert(and(fals, fals).value.get == 0)
    assert(and(fals,  tru).value.get == 0)
    assert(and( tru,  tru).value.get == 1)

    assertThrows[AssertionError] {
      and(tru, BVValueSummary(4, 4))
    }
    assertThrows[AssertionError] {
      and(BVValueSummary(4, 4), tru)
    }

    val a = BVSymbol("a", 1)
    val b = BVSymbol("b", 1)
    val a_and_b = BVAnd(a, b)
    assert(and(BVValueSummary(a), BVValueSummary(b)).symbolic == a_and_b)
  }

  it should "support or" in {
    val tru = BVValueSummary(true)
    val fals = BVValueSummary(false)

    def or(a: BVValueSummary, b: BVValueSummary): BVValueSummary = {
      BVValueSummary.binary(a, b, BVOr(_, _))
    }

    assert(or( tru, fals).value.get == 1)
    assert(or(fals, fals).value.get == 0)
    assert(or(fals,  tru).value.get == 1)
    assert(or( tru,  tru).value.get == 1)

    assertThrows[AssertionError] {
      or(tru, BVValueSummary(4, 4))
    }
    assertThrows[AssertionError] {
      or(BVValueSummary(4, 4), tru)
    }

    val a = BVSymbol("a", 1)
    val b = BVSymbol("b", 1)
    val a_or_b = BVOr(a, b)
    assert(or(BVValueSummary(a), BVValueSummary(b)).symbolic == a_or_b)
  }

  /*
  it should "simplify boolean operations" in {
    val tru = BVValueSummary(true)
    val fals = BVValueSummary(false)
    val a = BVValueSummary(smt.Symbol("a", smt.BoolType))
    val not_a = a.not()

    // a | 1 = 1
    tru.or(a).isConcrete should be (true)
    tru.or(a).concrete should be (BigInt(1))
    a.or(tru).concrete should be (BigInt(1))

    // a | 0 = a
    fals.or(a).symbolic should be (a.symbolic)
    a.or(fals).symbolic should be (a.symbolic)

    // a & 0 = 0
    fals.and(a).isConcrete should be (true)
    fals.and(a).concrete should be (BigInt(0))
    a.and(fals).concrete should be (BigInt(0))

    // a & 1 = a
    tru.and(a).symbolic should be (a.symbolic)
    a.and(tru).symbolic should be (a.symbolic)

    // a == 1 = a
    tru.cmpEqual(a).symbolic should be (a.symbolic)
    a.cmpEqual(tru).symbolic should be (a.symbolic)

    // a == 0 = not(a)
    fals.cmpEqual(a).symbolic should be (not_a.symbolic)
    a.cmpEqual(fals).symbolic should be (not_a.symbolic)

    // a != 1 = not(a)
    tru.cmpNotEqual(a).symbolic should be (not_a.symbolic)
    a.cmpNotEqual(tru).symbolic should be (not_a.symbolic)

    // a != 0 = a
    fals.cmpNotEqual(a).symbolic should be (a.symbolic)
    a.cmpNotEqual(fals).symbolic should be (a.symbolic)

    // not(not(a)) = a
    a.not().not().symbolic should be (a.symbolic)
  }

  it should "discard infeasible mux args" in {
    implicit val ctx: SymbolicContext = new SymbolicContext(
      // disable use of cache to make sure SMT solver is actually called in BVValueSummary.ite
      QuerySmtCacheInSmtToBdd = false,
      CheckITEConditionWithSmtSolver = true
    )

    val a = BVValueSummary(ctx.makeBitVector("a", 4))
    val zero = BVValueSummary(0, 4)
    val infeasible_cond = a.cmpNotEqual(zero).and(a.cmpEqual(zero))

    // make sure that it is actually infeasible
    ctx.isUnSat(infeasible_cond) should be (true)

    val x = BVValueSummary(ctx.makeBitVector("x", 4))
    val select_zero = BVValueSummary.ite(infeasible_cond, x, zero)
    // the ite should return zero
    select_zero.isConcrete should be (true)
    select_zero.concrete should be (0)
  }


   */
}
