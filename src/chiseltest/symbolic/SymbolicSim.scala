// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.mc
import maltese.mc._
import maltese.passes.{DeadCodeElimination, Inline, Pass, PassManager, Simplify}
import maltese.sym._

import java.io.File


class SymbolicSim(sys: TransitionSystem, ignoreAsserts: Boolean) {
  // we do not support assumptions at the moment
  private val assumptions =  sys.signals.filter(_.lbl == IsConstraint)
  require(assumptions.isEmpty, "Assumptions are currently not supported!")

  // we need to assertion names if we want to check them
  private val assertions = sys.signals.filter(_.lbl == IsBad).map(_.name)

  private val engine = SymEngine(sys, noInit = false)
  private var cycleCount = 0
  doCheckAsserts() // check assertions in initial state

  def step(): Unit = {
    cycleCount += 1
    doCheckAsserts()
  }

  def peek(signal: String): Value = {
    new Value(engine.signalAt(signal, cycleCount))
  }

  def poke(signal: String, value: Value): Unit = {
    engine.set(signal, cycleCount, Value.getValueSummary(value))
  }

  def makeSymbol(name: String, width: Int): Value = {
    new Value(engine.makeSymbol(name, width))
  }

  def assert(v: Value, msg: => String = ""): Unit = {
    val e = Value.getValueSummary(v)
    val isSat = Value.getValueSummary(v).isSat
    if(isSat) {
      throw new RuntimeException(s"Assertion failed! $msg")
    }
  }

  private def doCheckAsserts(): Unit = if(!ignoreAsserts) {
    assertions.foreach{ a =>
      assert(peek(a), s"$a in cycle $cycleCount")
    }
  }
}


class Value(private val e: BVValueSummary) {
  def isSymbolic: Boolean = e.isSymbolic
  def isConcrete: Boolean = e.isConcrete
  override def toString = e.value match {
    case Some(value) => "Value(" + value + ")"
    case None => "Value(" + e.toString + ")"
  }
}

private object Value {
  def getValueSummary(v: Value): BVValueSummary = v.e
}


object SymbolicSim {
  // TODO: add function to start with firrtl or a Chisel circuit

  def loadBtor(filename: String): SymbolicSim = loadBtor(filename, false)
  def loadBtor(filename: String, ignoreAsserts: Boolean): SymbolicSim = {
    // load transition system from file
    val sys = mc.Btor2.load(new File(filename))
    val simplified = simplifySystem(sys)
    new SymbolicSim(simplified, ignoreAsserts = ignoreAsserts)
  }

  def loadBtor(src: String, name: String): SymbolicSim = loadBtor(src, name, false)
  def loadBtor(src: String, name: String, ignoreAsserts: Boolean): SymbolicSim = {
    val sys = mc.Btor2.read(src = src, defaultName = name)
    val simplified = simplifySystem(sys)
    if(verbose) { println(simplified.serialize) }
    new SymbolicSim(simplified, ignoreAsserts = ignoreAsserts)
  }

  private val verbose = false
  private def simplifySystem(sys: TransitionSystem): TransitionSystem = PassManager(passes).run(sys, trace = verbose)
  private val passes: Iterable[Pass] = Seq(
    Simplify,
    new Inline,
    new DeadCodeElimination(removeUnusedInputs = true),

    Simplify,
    new Inline,
    new DeadCodeElimination(removeUnusedInputs = true),

    Simplify,
    // PrintSystem,
  )
}
