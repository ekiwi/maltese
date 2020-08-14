// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes

import maltese.smt._
import scala.collection.mutable

/** Inlines signals that are used only ones or that leaf expressions (symbols or constants) */
object InliningPass {

  def run(sys: TransitionSystem): TransitionSystem = {
    val doInline = findSignalsToInline(sys)
    if(doInline.isEmpty) {
      sys
    } else {
      val (inline, keep) = sys.signals.partition(s => doInline.contains(s.name))
      val inlineExpr = inline.map(s => s.name -> s.e).toMap
      def replace(e: SMTExpr): SMTExpr = SMTExprVisitor.map(replaceSymbols(inlineExpr.get)(_))(e)
      val remainingSignals = keep.map(s => s.copy(e = replace(s.e)))
      sys.copy(signals = remainingSignals)
    }
  }

  private def replaceSymbols(m: String => Option[SMTExpr])(e: SMTExpr): SMTExpr = e match {
    case s @ BVSymbol(name, _) => m(name).getOrElse(s)
    case s @ ArraySymbol(name, _, _) => m(name).getOrElse(s)
    case other => other
  }

  private def findSignalsToInline(sys: TransitionSystem): Set[String] = {
    // count how often a symbol is used
    val useCount = mutable.HashMap[String, Int]().withDefaultValue(0)
    val exprs = sys.signals.map(_.e) ++ sys.states.flatMap(s => s.init ++ s.next)
    exprs.foreach(e => SMTExprVisitor.foreach(countUses(useCount))(e))
    val onlyUsedOnce = useCount.collect { case (name, count) if count <= 1 => name }.toSet
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

  private def countUses(useCount: mutable.Map[String, Int])(e: SMTExpr): Unit = e match {
    case BVSymbol(name, _) => useCount(name) += 1
    case ArraySymbol(name, _, _) => useCount(name) += 1
    case _ =>
  }


}
