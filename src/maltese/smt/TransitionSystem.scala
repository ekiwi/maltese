// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

case class State(sym: SMTSymbol, init: Option[SMTExpr], next: Option[SMTExpr])
case class Signal(name: String, e: SMTExpr, lbl: SignalLabel = IsNode)
case class TransitionSystem(name: String, inputs: Seq[BVSymbol], states: Seq[State], signals: Seq[Signal])

object TransitionSystem {
  def serialize(sys: TransitionSystem): String = {
    (Iterator(sys.name) ++
    sys.inputs.map(i => s"input ${i.name} : ${SMTExpr.serializeType(i)}") ++
    sys.signals.map(s => s"${SignalLabel.labelToString(s.lbl)} ${s.name} : ${SMTExpr.serializeType(s.e)} = ${s.e}") ++
    sys.states.map(s => s"state ${s.sym} = [init] ${s.init} [next] ${s.next}")
      ).mkString("\n")
  }
}

sealed trait SignalLabel
case object IsNode extends SignalLabel
case object IsOutput extends SignalLabel
case object IsConstraint extends SignalLabel
case object IsBad extends SignalLabel
case object IsFair extends SignalLabel

object SignalLabel {
  private val labels = Seq(IsNode, IsOutput, IsConstraint, IsBad, IsFair)
  private val labelStrings = Seq("node", "output", "constraint", "bad", "fair")
  val labelToString: SignalLabel => String = labels.zip(labelStrings).toMap
  val stringToLabel: String => SignalLabel = labelStrings.zip(labels).toMap
}