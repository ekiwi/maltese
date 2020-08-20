// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.exec


import net.sf.javabdd._
import maltese.smt._
import scala.collection.mutable


class BDDToSMTConverter(
   bdds: BDDFactory = JFactory.init(100, 100),
   ConvertBooleanOpsInSmtToBdd: Boolean = true) {
  private var bddVarCount = 0
  private val smtToBddCache = mutable.HashMap[BVExpr, BDD]()
  private val bddLiteralToSmt = mutable.HashMap[Int, BVExpr]()

  val tru : BDD = bdds.one()
  val fals : BDD = bdds.zero()

  def smtToBdd(expr: BVExpr) : BDD = {
    assert(expr.width == 1, s"can only convert 1-bit expressions to BDD, but `$expr` is ${expr.width}-bit")
    if(!smtToBddCache.contains(expr)) {
      if (ConvertBooleanOpsInSmtToBdd) expr match {
        case BVNot(a) => return smtToBdd(a).not()
        case BVOp(Op.And, a, b) => return smtToBdd(a).and(smtToBdd(b))
        case BVOp(Op.Or, a, b) => return smtToBdd(a).or(smtToBdd(b))
        case BVOp(Op.Xor, a, b) => return smtToBdd(a).xor(smtToBdd(b))
        case BVEqual(a, b) => return smtToBdd(a).biimp(smtToBdd(b))
        case _ => None
      }

      if (bdds.varNum() <= bddVarCount) {
        bdds.setVarNum(bddVarCount + 1)
      }
      smtToBddCache(expr) = bdds.ithVar(bddVarCount)
      bddLiteralToSmt(bddVarCount) = expr
      bddVarCount += 1
    }
    smtToBddCache(expr)
  }

  def bddToSmt(bdd: BDD) : BVExpr = {
    if(bdd.isOne) { True() }
    else if(bdd.isZero) { False() }
    else {
      // all cases verified with sympy:
      // simplify_logic(ITE(c, 1, 0)) = c
      // simplify_logic(ITE(c, 0, 1)) = ~c
      // simplify_logic(ITE(c, 1, b)) = b | c
      // simplify_logic(ITE(c, 0, b)) = b & ~c
      // simplify_logic(ITE(c, b, 1)) = b | ~c
      // simplify_logic(ITE(c, b, 0)) = b & c
      val high_is_one  = bdd.high().isOne
      val high_is_zero = bdd.high().isZero
      val low_is_one   = bdd.low().isOne
      val low_is_zero  = bdd.low().isZero
      val is_var = high_is_one && low_is_zero
      val is_neg_var = high_is_zero && low_is_one
      val is_or_var = high_is_one
      val is_and_neg_var = high_is_zero
      val is_or_neg_var = low_is_one
      val is_and_var = low_is_zero

      val cond = bddLiteralToSmt(bdd.`var`())
      lazy val neg_cond = BVNot(cond)

      if(is_var) { cond }
      else if(is_neg_var) { neg_cond }
      else if(is_or_var || is_and_neg_var) {
        val b = bddToSmt(bdd.low())
        if(is_or_var) { BVOp(Op.Or, cond, b) }
        else          { BVOp(Op.And, neg_cond, b) }
      }
      else if(is_or_neg_var || is_and_var) {
        val b = bddToSmt(bdd.high())
        if(is_or_neg_var) { BVOp(Op.Or, neg_cond, b) }
        else              { BVOp(Op.And, cond, b) }
      }
      else {
        val tru = bddToSmt(bdd.high())
        val fal = bddToSmt(bdd.low())
        val args = cond match {
          case BVNot(n_cond) => List(n_cond, fal, tru)
          case _ => List(cond, tru, fal)
        }
        BVIte(args(0), args(1), args(2))
      }
    }
  }

  def reset(): Unit = {
    bddVarCount = 0
    smtToBddCache.clear()
    bddLiteralToSmt.clear()
    bdds.reset()
  }

  // we ignore the cached true/false expressions
  def getCacheSize: Int = smtToBddCache.size - 2
}
