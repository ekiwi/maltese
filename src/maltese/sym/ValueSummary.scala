// Copyright 2020-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.sym

import maltese.smt._
import com.github.javabdd.BDD

sealed trait ValueSummary {
  def size: Int
  def isSymbolic: Boolean = !isConcrete
  def isConcrete: Boolean
}

object BVValueSummary {
  def apply(expr: BVExpr)(implicit ctx: SymbolicContext): BVValueSummary =
    new BVValueSummary(ctx, List(BVEntry(ctx.tru, expr)))
  def apply(value: BigInt, width: Int)(implicit ctx: SymbolicContext): BVValueSummary =
    apply(BVLiteral(value, width))
  def apply(value: Boolean)(implicit ctx: SymbolicContext): BVValueSummary =
    apply(if(value) 1 else 0, 1)

  def binary(a: BVValueSummary, b: BVValueSummary, op: (BVExpr, BVExpr) => BVExpr): BVValueSummary = {
    assert(a.ctx.eq(b.ctx))

    val pairs = for(e1 <- a.entries; e2 <- b.entries) yield { (e1.guard.and(e2.guard), e1.value, e2.value) }
    // we filter out pairs for which the guard is false
    val feasiblePairs = pairs.filterNot(_._1.isZero)
    // TODO: should be coalesce only for ITE operations or also for boolean ops?
    val rawEntries = feasiblePairs.map { case (guard, a, b) => BVEntry(guard, simplify(op(a, b))) }
    val newEntries = if(a.ctx.opts.DoNotCoalesce) { rawEntries } else { coalesce(rawEntries) }
    importIntoGuard(new BVValueSummary(a.ctx, newEntries))
  }

  def unary(a: BVValueSummary, op: BVExpr => BVExpr): BVValueSummary = {
    // all we need to do is to apply the operation to all entries + call simplify
    val newEntries = a.entries.map(e => e.copy(value = simplify(op(e.value))))
    importIntoGuard(new BVValueSummary(a.ctx, newEntries))
  }

  def ite(cond: BVValueSummary, tru: => BVValueSummary, fals: => BVValueSummary): BVValueSummary = {
    // if the condition is concrete, we only need to evaluate one side of the mux
    if(cond.isTrue) { assert(cond.ctx.eq(tru.ctx)) ; tru
    } else if(cond.isFalse) { assert(cond.ctx.eq(fals.ctx)) ; fals
    } else {
      val ctx = cond.ctx
      assert(ctx.eq(tru.ctx) && ctx.eq(fals.ctx))
      // TODO: integrate SMT solver for better filtering
      val isFalse: BDD => Boolean =
        if(ctx.opts.CheckITEConditionWithSmtSolver) { b => b.isZero || ctx.isUnSat(b) } else { b => b.isZero }

      // find all conditions under which the true/false branch will be taken
      val truCond  = cond.entries.map(e => ctx.smtToBdd(e.value).and(e.guard)).filterNot(isFalse)
      val falsCond = cond.entries.map(e => ctx.smtToBdd(e.value).not().and(e.guard)).filterNot(isFalse)

      def combine(conds: List[BDD], entries: List[BVEntry]): List[BVEntry] =
        conds.flatMap(c => entries.flatMap{ e =>
          val combinedGuard = c.and(e.guard)
          if(isFalse(combinedGuard)) { None } else { Some(e.copy(guard = combinedGuard)) }
        })

      val rawEntries = combine(truCond, tru.entries) ++ combine(falsCond, fals.entries)
      val newEntries = if(ctx.opts.DoNotCoalesce) { rawEntries } else { coalesce(rawEntries) }
      importIntoGuard(new BVValueSummary(ctx, newEntries))
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


  private def importIntoGuard(v: BVValueSummary): BVValueSummary =
    if(v.width != 1 || !v.ctx.opts.ImportBooleanExpressionsIntoGuard) {
      v
    } else {
      new BVValueSummary(v.ctx, importIntoGuard(v.ctx, v.entries))
    }

  private def importIntoGuard(ctx: SymbolicContext, entries: List[BVEntry]): List[BVEntry] = {
    val tru = entries.map(e => e.guard.and(ctx.smtToBdd(e.value))).reduce((a,b) => a.or(b))
    // putting the false entry first gives us a nicer print out when converting to SMT and serializing in `toString`
    val newEntries = List(BVEntry(tru.not(), False()), BVEntry(tru, True())).filterNot(_.guard.isZero)
    assert(newEntries.nonEmpty)
    newEntries
  }

  private def toSMT(v: BVValueSummary): BVExpr = {
    if(v.entries.size == 1) {
      v.entries.head.value
    } else if(v.width == 1) { // for 1-bit values, we just build a boolean formula
      val ctx = v.ctx
      val tru = v.entries.map(e => e.guard.and(ctx.smtToBdd(e.value))).reduce((a,b) => a.or(b))
      ctx.bddToSmt(tru)
    } else {
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
  override def toString = if(size < 100) {
    SMTSimplifier.simplify(BVValueSummary.toSMT(this)).toString
  } else { s"ValueSummary w/ $size entries" }
  override def size = entries.size
  override def isConcrete = (size == 1) && entries.head.value.isInstanceOf[BVLiteral]
  def value: Option[BigInt] = if(isConcrete) {
    Some(entries.head.value.asInstanceOf[BVLiteral].value)
  } else { None }
  def symbolic: BVExpr = BVValueSummary.toSMT(this)
  def isValid: Boolean = ctx.isValid(this)
  def isSat: Boolean = ctx.isSat(this)
  def entryCount: Int = entries.size
}