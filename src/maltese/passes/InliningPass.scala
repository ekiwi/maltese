// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes

import maltese.smt._
import scala.collection.mutable

/** Inlines signals that are used only ones or that leaf expressions (symbols or constants) */
// TODO: don't inline things into the state next and init functions, try to rename them to something sensible.
object InliningPass {

  def run(sys: TransitionSystem): TransitionSystem = {
    val doInline = findSignalsToInline(sys)
    if(doInline.isEmpty) {
      sys
    } else {
      val inlineExpr = mutable.HashMap[String, SMTExpr]()
      val remainingSignals = sys.signals.flatMap { signal =>
        val inlinedE = SMTExprVisitor.map(replaceSymbols(inlineExpr.get)(_))(signal.e)
        if(doInline.contains(signal.name)) {
          inlineExpr(signal.name) = inlinedE
          None
        } else {
          Some(signal.copy(e = inlinedE))
        }
      }
      def replace(e: SMTExpr): SMTExpr = SMTExprVisitor.map(replaceSymbols(inlineExpr.get)(_))(e)
      val states = sys.states.map(s => s.copy(init = s.init.map(replace), next = s.next.map(replace)))
      sys.copy(signals = remainingSignals, states = states)
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
