// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.smt
import org.scalatest.flatspec.AnyFlatSpec

class ServTests extends AnyFlatSpec {
  behavior.of("Chisel re-implementation of serv")

  it should "execute symbolically" in {
    val sim = SymbolicSim.loadFirrtlFile(
      "benchmarks/riscv/serv/ServTopWithRam.lo.fir",
      "benchmarks/riscv/serv/ServTopWithRam.anno.json"
    )

    // set all inputs to a concrete value and reset design
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()

  }
}
