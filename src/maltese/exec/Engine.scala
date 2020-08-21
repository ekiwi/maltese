// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.exec

import maltese.smt._
import scala.collection.mutable

object Engine {
  def apply(sys: TransitionSystem): Engine = new Engine(sys)
}

class Engine private(sys: TransitionSystem) {
  private val inputs = sys.inputs.map(i => i.name -> i).toMap
  private val states = sys.states.map(s => s.sym.name -> s).toMap
  private val signals = sys.signals.map(s => s.name -> s).toMap
  private val results = mutable.ArrayBuffer[mutable.HashMap[String, BVValueSummary]]()
  private implicit val ctx = new SymbolicContext()

  def signalAt(name: String, step: Int): BVValueSummary = {
    if(results.size < step + 1) {
      (0 to (step - results.size)).foreach(_ => results.append(mutable.HashMap()))
    }
    val frame = results(step)
    if(!frame.contains(name)) {
      frame(name) = computeSignalAt(name, step)
    }
    val r = frame(name)
    if(r.size > 1000) {
      println(s"WARN: $name@$step.size = ${r.size} > 1k")
    }
    // println(s"$name@$step: $r")
    r
  }

  private def computeSignalAt(name: String, step: Int): BVValueSummary = {
    if(signals.contains(name)) {
      eval(signals(name).e.asInstanceOf[BVExpr], step)
    } else if(inputs.contains(name)) {
      inputAt(name, step)
    } else if(states.contains(name)) {
      stateAt(name, step)
    } else {
      throw new RuntimeException(s"Unknown signal $name @ $step")
    }
  }
  private def eval(expr: BVExpr, step: Int): BVValueSummary = expr match {
    case BVSymbol(name, _) => signalAt(name, step)
    case l : BVLiteral => BVValueSummary(l)
    case u : BVUnaryExpr => BVValueSummary.unary(eval(u.e, step), u.reapply)
    case u : BVBinaryExpr => BVValueSummary.binary(eval(u.a, step), eval(u.b, step), u.reapply)
    case BVIte(cond, tru, fals) => BVValueSummary.ite(eval(cond, step), eval(tru, step), eval(fals, step))
    case other => throw new RuntimeException(s"Unexpected expression: $other")
  }


  private def stateAt(name: String, step: Int): BVValueSummary = {
    val state = states(name)
    if(step == 0) {
      if(state.init.isDefined) {
        signalAt(name + ".init", 0)
      } else {
        val sym = symbols.getOrElseUpdate(name + "@0", SMTSymbol.fromExpr(name + "@0", state.sym).asInstanceOf[BVSymbol])
        BVValueSummary(sym)
      }
    } else {
      assert(step > 0)
      signalAt(name + ".next", step - 1)
    }
  }

  private def inputAt(name: String, step: Int): BVValueSummary = {
    val id = name + "@" + step
    val sym = symbols.getOrElseUpdate(id, BVSymbol(id, inputs(name).width))
    BVValueSummary(sym)
  }
  private def symbols = mutable.HashMap[String, BVSymbol]()
}
