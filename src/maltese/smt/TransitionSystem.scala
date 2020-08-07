// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

case class State(sym: SMTSymbol, init: Option[SMTExpr], next: Option[SMTExpr])
case class Signal(name: Option[String], e: BVExpr)
case class TransitionSystem(
 name: String, inputs: Seq[BVSymbol], states: Seq[State], signals: Seq[Signal],
 outputs: Set[Int], constraints: Set[Int], bad: Set[Int], fair: Set[Int])