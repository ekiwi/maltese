// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes

import maltese.smt._

/** Removed all signals that are not used. */
object DeadCodeElimination extends Pass {
  override def name: String = "DeadCodeElimination"

  override def run(sys: TransitionSystem): TransitionSystem = {
    val useCount = Analysis.countUses(sys.signals)
    val signals = sys.signals.filterNot(s => useCount(s.name) == 0 && s.lbl == IsNode)
    sys.copy(signals = signals)
  }
}
