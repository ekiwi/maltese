// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.mc

import maltese.{mc, smt}
import maltese.smt.{ArraySymbol, BVExpr, BVSymbol, SMTExpr, SMTSymbol}

sealed trait State {
  def sym: SMTSymbol
  def init: Option[SMTSymbol]
  def next: Option[SMTSymbol]
  def name: String = sym.name
  def rename(newName: String): State
  def addInit(i: SMTSymbol): State
  def addNext(n: SMTSymbol): State
  def map(f: SMTExpr => SMTExpr): State
}
object State {
  def apply(sym: SMTSymbol): State = sym match {
    case b: BVSymbol => BVState(b, None, None)
    case a: ArraySymbol => ArrayState(a, None, None)
  }
}
case class BVState(sym: BVSymbol, init: Option[BVSymbol], next: Option[BVSymbol]) extends State {
  override def rename(newName: String): BVState = copy(sym = sym.rename(newName))
  override def addInit(i: SMTSymbol): BVState = { assert(init.isEmpty) ; copy(init = Some(i.asInstanceOf[BVSymbol])) }
  override def addNext(n: SMTSymbol): BVState = { assert(next.isEmpty) ; copy(next = Some(n.asInstanceOf[BVSymbol])) }
  override def map(f: SMTExpr => SMTExpr): BVState = BVState(
    f(sym).asInstanceOf[BVSymbol], init.map(f).map(_.asInstanceOf[BVSymbol]), next.map(f).map(_.asInstanceOf[BVSymbol])
  )
}
case class ArrayState(sym: ArraySymbol, init: Option[ArraySymbol], next: Option[ArraySymbol]) extends State {
  override def rename(newName: String): ArrayState = copy(sym = sym.rename(newName))
  override def addInit(i: SMTSymbol): ArrayState = { assert(init.isEmpty) ; copy(init = Some(i.asInstanceOf[ArraySymbol])) }
  override def addNext(n: SMTSymbol): ArrayState = { assert(next.isEmpty) ; copy(next = Some(n.asInstanceOf[ArraySymbol])) }
  override def map(f: SMTExpr => SMTExpr): ArrayState = ArrayState(
    f(sym).asInstanceOf[ArraySymbol], init.map(f).map(_.asInstanceOf[ArraySymbol]), next.map(f).map(_.asInstanceOf[ArraySymbol])
  )
}
case class Signal(name: String, e: SMTExpr, lbl: SignalLabel = IsNode) {
  def toSymbol: SMTSymbol = SMTSymbol.fromExpr(name, e)
  def sym: SMTSymbol = toSymbol
}
case class TransitionSystem(name: String, inputs: List[BVSymbol], states: List[State], signals: List[Signal]) {
  def serialize: String = TransitionSystem.serialize(this)
}

sealed trait SignalLabel
case object IsNode extends SignalLabel
case object IsOutput extends SignalLabel
case object IsConstraint extends SignalLabel
case object IsBad extends SignalLabel
case object IsFair extends SignalLabel
case object IsNext extends SignalLabel
case object IsInit extends SignalLabel

object SignalLabel {
  private val labels = Seq(IsNode, IsOutput, IsConstraint, IsBad, IsFair, IsNext, IsInit)
  val labelStrings = Seq("node", "output", "constraint", "bad", "fair", "next", "init")
  val labelToString: SignalLabel => String = labels.zip(labelStrings).toMap
  val stringToLabel: String => SignalLabel = labelStrings.zip(labels).toMap
}

object TransitionSystem {
  def serialize(sys: TransitionSystem): String = {
    (Iterator(sys.name) ++
      sys.inputs.map(i => s"input ${i.name} : ${SMTExpr.serializeType(i)}") ++
      sys.signals.map(s => s"${SignalLabel.labelToString(s.lbl)} ${s.name} : ${SMTExpr.serializeType(s.e)} = ${s.e}") ++
      sys.states.map(serialize)
      ).mkString("\n")
  }

  def serialize(s: State): String = {
    s"state ${s.sym.name} : ${SMTExpr.serializeType(s.sym)}" +
      (s.init match { case None => "" case Some(init) => s"\n  [init] ${init}" }) +
      (s.next match { case None => "" case Some(next) => s"\n  [next] ${next}"})
  }

  /** prefixes all signal names with the name of the transition system */
  def prefixSignals(sys: TransitionSystem): TransitionSystem = {
    val prefix = sys.name + "."
    val names: Iterable[String] = sys.inputs.map(_.name) ++ sys.states.map(_.sym.name) ++ sys.signals.map(_.name)
    val renames = names.map(n => n -> (prefix + n)).toMap
    val inputs = sys.inputs.map(i => i.rename(renames.getOrElse(i.name, i.name)))
    def r(e: SMTExpr): SMTExpr = rename(renames)(e)
    val states = sys.states.map(_.map(r))
    val signals = sys.signals.map(s => s.copy(name = renames(s.name), e = r(s.e)))
    TransitionSystem(sys.name, inputs, states, signals)
  }

  private def rename(map: Map[String, String])(e: SMTExpr): SMTExpr = e match {
    case sym : SMTSymbol => sym.rename(map.getOrElse(sym.name, sym.name))
    case other => other.mapExpr(rename(map))
  }

  def combine(name: String, sys: List[TransitionSystem]): TransitionSystem = {
    TransitionSystem(name,
      inputs = sys.flatMap(_.inputs),
      states = sys.flatMap(_.states),
      signals = sys.flatMap(_.signals)
    )
  }

  /** combines all properties into one */
  def combineProperties(sys: TransitionSystem): TransitionSystem = {
    ???
  }

  def connect(sys: TransitionSystem, cons: Map[String, BVExpr]): TransitionSystem = {
    // ensure that the ports exists
    cons.foreach(i => assert(sys.inputs.exists(_.name == i._1), s"Cannot connect to non-existing port ${i._1}"))
    // filter out inputs
    val inputs = sys.inputs.filterNot(i => cons.contains(i.name))
    val connections = cons.map(c => mc.Signal(c._1, c._2)).toList
    sys.copy(inputs = inputs, signals = connections ++ sys.signals)
  }

  // replaces states with connections to a signal
  def connectStates(sys: TransitionSystem, cons: Map[String, SMTExpr]): TransitionSystem = {
    // ensure that the ports exists
    cons.foreach(i => assert(sys.states.exists(_.name == i._1), s"Cannot connect to non-existing state ${i._1}"))
    // filter out inputs
    val states = sys.states.filterNot(s => cons.contains(s.name))
    val connections = cons.map(c => mc.Signal(c._1, c._2)).toList
    sys.copy(states = states, signals = connections ++ sys.signals)
  }

  def systemExpressions(sys: TransitionSystem): Iterable[smt.SMTExpr] =
    sys.signals.map(_.e) ++ sys.states.flatMap(s => s.init ++ s.next)

  def findUFs(sys: TransitionSystem): Iterable[smt.DeclareFunction] = {
    val calls = systemExpressions(sys).flatMap(findUFCalls)
    // find unique functions
    calls.groupBy(_.sym.name).map(_._2.head)
  }

  private def findUFCalls(e: smt.SMTExpr): List[smt.DeclareFunction] = {
    val f = e match {
      case smt.BVFunctionCall(name, args, width) =>
        Some(smt.DeclareFunction(smt.BVSymbol(name, width), args))
      case smt.ArrayFunctionCall(name, args, indexWidth, dataWidth) =>
        Some(smt.DeclareFunction(smt.ArraySymbol(name, indexWidth, dataWidth), args))
      case _ => None
    }
    f.toList ++ e.children.flatMap(findUFCalls)
  }

  def analyzeFeatures(sys: TransitionSystem): TransitionSystemFeatures =
    systemExpressions(sys).map(analyzeFeatures).reduce((a,b) => a | b)

  def analyzeFeatures(e: smt.SMTExpr): TransitionSystemFeatures = {
    val info = e match {
      case fa: smt.BVForall => Some(HasQuantifier)
      case a: smt.ArrayExpr => Some(HasArrays)
      case _: smt.BVFunctionCall | _: smt.ArrayFunctionCall => Some(HasUF)
      case _ => None
    }
    (e.children.map(analyzeFeatures) ++ info).foldLeft(Base)((a,b) => a | b)
  }
  private val HasQuantifier = TransitionSystemFeatures(true, false, false)
  private val HasArrays = TransitionSystemFeatures(false, true, false)
  private val HasUF = TransitionSystemFeatures(false, false, true)
  private val Base = TransitionSystemFeatures(false, false, false)
}

case class TransitionSystemFeatures(hasQuantifiers: Boolean, hasArrays: Boolean, hasUF: Boolean) {
  def |(other: TransitionSystemFeatures): TransitionSystemFeatures =
    TransitionSystemFeatures(hasQuantifiers | other.hasQuantifiers, hasArrays | other.hasArrays, hasUF | other.hasUF)
}