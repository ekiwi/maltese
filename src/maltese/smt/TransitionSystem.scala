// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

case class State(sym: SMTSymbol, init: Option[SMTExpr], next: Option[SMTExpr])
case class Signal(name: String, e: SMTExpr, lbl: SignalLabel = IsNode)
case class TransitionSystem(name: String, inputs: Seq[BVSymbol], states: Seq[State], signals: Seq[Signal])

sealed trait SignalLabel
case object IsNode extends SignalLabel
case object IsOutput extends SignalLabel
case object IsConstraint extends SignalLabel
case object IsBad extends SignalLabel
case object IsFair extends SignalLabel

object SignalLabel {
  private val labels = Seq(IsNode, IsOutput, IsConstraint, IsBad, IsFair)
  private val labelStrings = Seq("node", "output", "constraint", "bad", "fair")
  val labelToString = labels.zip(labelStrings).toMap
  val stringToLabel = labelStrings.zip(labels).toMap
}