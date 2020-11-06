// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.passes

import maltese.mc.TransitionSystem

/** Prints the system to stdout without modifying it */
object PrintSystem extends Pass {
  override def name: String = "PrintSystem"

  override def run(sys: TransitionSystem): TransitionSystem = {
    println(sys.serialize)
    println()
    sys
  }
}
