package chiseltest.symbolic

import maltese.mc._
import maltese.sym._


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
    engine.
  }
}


class Value(e: BVValueSummary) {
  def isSymbolic: Boolean = e.isSymbolic
  def isConcrete: Boolean = e.isConcrete
}

object UIntSymbol {
  def apply(name: String, width: Int): Value = {
    // TODO: how do we determine the context?
    ???
  }
}



object SymbolicSim {
  def loadBtor(filename: String): SymbolicSim = {

  }
}
