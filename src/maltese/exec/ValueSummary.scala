// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.exec

import maltese.smt._
import net.sf.javabdd.{BDD, JFactory}

sealed trait ValueSummary {
  def size: Int
}

object BVValueSummary {
  def apply(expr: BVExpr)(implicit ctx: SymbolicContext): BVValueSummary
  = new BVValueSummary(ctx, List(BVEntry(ctx.tru, expr)))

  def binary(a: BVValueSummary, b: BVValueSummary, op: (BVExpr, BVExpr) => BVExpr): BVValueSummary = {
    assert(a.ctx.eq(b.ctx))
    val ctx = a.ctx

    val pairs = for(e1 <- a.entries; e2 <- b.entries) yield { (e1.guard.and(e2.guard), e1.value, e2.value) }
    val newEntries = pairs.map { case (guard, a, b) => BVEntry(guard, simplify(op(a, b))) }
    new BVValueSummary(a.ctx, newEntries)
  }

  def unary(a: BVValueSummary, op: BVExpr => BVExpr): BVValueSummary = {
    // all we need to do is to apply the operation to all entries + call simplify
    val newEntries = a.entries.map(e => e.copy(value = simplify(op(e.value))))
    new BVValueSummary(a.ctx, newEntries)
  }

  def ite(cond: BVValueSummary, tru: => BVValueSummary, fals: => BVValueSummary): BVValueSummary = {
    // if the condition is concrete, we only need to evaluate one side of the mux
    if(cond.isTrue) { assert(cond.ctx.eq(tru.ctx)) ; tru
    } else if(cond.isFalse) { assert(cond.ctx.eq(fals.ctx)) ; fals
    } else {
      val ctx = cond.ctx
      assert(ctx.eq(tru.ctx) && ctx.eq(fals.ctx))
      // TODO: integrate SMT solver for better filtering
      val isFalse: BDD => Boolean = { b => b.isZero }

      // find all conditions under which the true/false branch will be taken
      val truCond  = cond.entries.map(e => ctx.smtToBdd(e.value).and(e.guard)).filterNot(isFalse)
      val falsCond = cond.entries.map(e => ctx.smtToBdd(e.value).not().and(e.guard)).filterNot(isFalse)

      def combine(conds: List[BDD], entries: List[BVEntry]): List[BVEntry] =
        conds.flatMap(c => entries.flatMap{ e =>
          val combinedGuard = c.and(e.guard)
          if(isFalse(combinedGuard)) { None } else { Some(e.copy(guard = combinedGuard)) }
        })

      val rawEntries = combine(truCond, tru.entries) ++ combine(falsCond, fals.entries)
      val newEntries = if(ctx.DoNotCoalesce) { rawEntries } else { coalesce(rawEntries) }
      new BVValueSummary(ctx, newEntries)
    }
  }

  private def coalesce(entries: List[BVEntry]) : List[BVEntry] = {
    val (duplicate, others) = entries.groupBy(_.value).partition(_._2.size > 1)
    if(duplicate.isEmpty) {
      entries
    } else {
      (duplicate.map { case (_, entries) =>
        val combinedGuard = entries.map(_.guard).reduce((a,b) => a.or(b))
        entries.head.copy(guard = combinedGuard)
      } ++ others.map(_._2.head)).toList
    }
  }

  // TODO: we could get better performance by only simplifying one new expression
  private def simplify(expr: BVExpr): BVExpr = SMTSimplifier.simplify(expr).asInstanceOf[BVExpr]

  private case class BVEntry(guard: BDD, value: BVExpr) {
    def width = value.width
  }

  private def toSMT(v: BVValueSummary): BVExpr = {
    if(v.entries.size == 1) { v.entries.head.value } else {
      v.entries.drop(1).foldLeft(v.entries.head.value) { (a: BVExpr, b: BVEntry) =>
        val cond = v.ctx.bddToSmt(b.guard)
        val args = cond match {
          case BVNot(n_cond) => List(n_cond, a, b.value)
          case _ => List(cond, b.value, a)
        }
        BVIte(args(0), args(1), args(2))
      }
    }
  }
}


class BVValueSummary private(private val ctx: SymbolicContext,
                             private val entries: List[BVValueSummary.BVEntry]) extends ValueSummary {
  assert(entries.nonEmpty)
  val width = entries.head.width
  assert(entries.forall(_.width == width))
  def isTrue: Boolean = entries.size == 1 && entries.head.value == True()
  def isFalse: Boolean = entries.size == 1 && entries.head.value == False()
  override def toString = BVValueSummary.toSMT(this).toString
  override def size = entries.size
}