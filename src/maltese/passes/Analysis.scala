// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.passes

import maltese.smt._
import scala.collection.mutable

object Analysis {
  def countUses(signals: Iterable[Signal]): String => Int = {
    // count how often a symbol is used
    val useCount = mutable.HashMap[String, Int]().withDefaultValue(0)
    val exprs = signals.map(_.e)
    exprs.foreach(e => SMTExprVisitor.foreach(countUses(useCount))(e))
    useCount
  }

  private def countUses(useCount: mutable.Map[String, Int])(e: SMTExpr): Unit = e match {
    case BVSymbol(name, _) => useCount(name) += 1
    case ArraySymbol(name, _, _) => useCount(name) += 1
    case _ =>
  }
}
