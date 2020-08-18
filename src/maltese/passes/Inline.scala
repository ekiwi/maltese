// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes

import maltese.smt._
import scala.collection.mutable

/** Inlines signals:
 * - if the signal is only used once
 * - if it is a lead expression (symbol or constant)
 * This pass does not remove any symbols.
 * Use DeadCodeElimination to get rid of any unused signals after inlining.
 */
object Inline extends Pass {
  override def name: String = "Inline"

  override def run(sys: TransitionSystem): TransitionSystem = {
    val doInline = findSignalsToInline(sys)
    if(doInline.isEmpty) {
      sys
    } else {
      val inlineExpr = mutable.HashMap[String, SMTExpr]()
      val signals = sys.signals.map { signal =>
        val inlinedE = SMTExprVisitor.map(replaceSymbols(inlineExpr.get)(_))(signal.e)
        if(doInline.contains(signal.name)) { inlineExpr(signal.name) = inlinedE }
        signal.copy(e = inlinedE)
      }
      sys.copy(signals = signals)
    }
  }

  private def replaceSymbols(m: String => Option[SMTExpr])(e: SMTExpr): SMTExpr = e match {
    case s @ BVSymbol(name, _) => m(name).getOrElse(s)
    case s @ ArraySymbol(name, _, _) => m(name).getOrElse(s)
    case other => other
  }

  private def findSignalsToInline(sys: TransitionSystem): Set[String] = {
    // count how often a symbol is used
    val useCount = Analysis.countUses(sys.signals)
    val onlyUsedOnce = sys.signals.filter(s => useCount(s.name) <= 1).map(_.name).toSet
    // we also want to inline signals that are only aliases
    val leafSignals = sys.signals.filter(s => isLeafExpr(s.e)).map(_.name).toSet
    // only regular node signals can be inlined
    val canBeInlined = sys.signals.filter(_.lbl == IsNode).map(_.name).toSet

    onlyUsedOnce.union(leafSignals).intersect(canBeInlined)
  }

  private def isLeafExpr(e: SMTExpr): Boolean = e match {
    case _: BVSymbol => true
    case _: BVLiteral => true
    case _: ArraySymbol => true
    case _: ArrayConstant => true
    case _ => false
  }
}
