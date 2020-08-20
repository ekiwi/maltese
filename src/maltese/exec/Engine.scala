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
  private val signals = sys.signals.map(s => s.name -> s).toMap
  private val results = mutable.ArrayBuffer[mutable.HashMap[String, BVValueSummary]]()

  def computeSignalAt(name: String, step: Int): BVValueSummary = {

  }

  private def eval(e: BVExpr, step: Int): BVValueSummary = e match {


  }



}
