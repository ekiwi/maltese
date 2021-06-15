// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.Maltese.simplifySystem
import maltese.mc
import maltese.mc._
import maltese.passes.{DeadCodeElimination, Inline, Pass, Simplify}
import maltese.sym._

import java.io.File


class SymbolicSim(sys: TransitionSystem) {
  private val engine = SymEngine(sys, noInit = false)
  private var cycleCount = 0

  def step(): Unit = {
    cycleCount += 1
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

  def assert(v: Value): Unit = {
    // TODO: check if always valid!
  }
}


class Value(private val e: BVValueSummary) {
  def isSymbolic: Boolean = e.isSymbolic
  def isConcrete: Boolean = e.isConcrete
}

private object Value {
  def getValueSummary(v: Value): BVValueSummary = v.e
}


object SymbolicSim {
  // TODO: add function to start with firrtl or a Chisel circuit

  def loadBtor(filename: String): SymbolicSim = {
    // load transition system from file
    val sys = mc.Btor2.load(new File(filename))
    val simplified = simplifySystem(sys)
    new SymbolicSim(simplified)
  }

  def loadBtor(src: String, name: String): SymbolicSim = {
    val sys = mc.Btor2.read(src = src, defaultName = name)
    val simplified = simplifySystem(sys)
    new SymbolicSim(simplified)
  }


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
