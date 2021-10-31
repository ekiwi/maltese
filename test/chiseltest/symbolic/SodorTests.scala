// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.smt
import org.scalatest.flatspec.AnyFlatSpec

class Sodor1StageTests extends AnyFlatSpec {
  behavior.of("unpipelined sodor")

  it should "execute symbolically" in {
    val sim = SymbolicSim.loadFirrtlFile("benchmarks/rfuzz/Sodor1Stage.fir", "", ignoreAsserts = true)

    // set all inputs to a concrete value
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()

    // provide an instruction
    sim.poke("io_imem_resp_valid", 1)
    sim.poke("io_imem_resp_bits_data", RiscV.symbolicAdd)

    // initialize the register file
    sim.poke("core.d.regfile", RiscV.BaseRegs)

    // there is only a single stage
    sim.step()

    val regsNext = sim.peek("core.d.regfile")
    RiscV.verifyAdd(regsNext.getSMT.asInstanceOf[smt.ArrayExpr])
  }
}

class Sodor3StageTests extends AnyFlatSpec {
  behavior.of("sodor w/ 3 stages")

  it should "execute symbolically" in {
    val sim = SymbolicSim.loadFirrtlFile("benchmarks/rfuzz/Sodor3Stage.fir", "", ignoreAsserts = true)

    // set all inputs to a concrete value
    sim.inputs.foreach(sim.poke(_, 0))
    // set all initial state (prior to reset) to a concrete random value
    sim.randomizeStates(skipInitialized = true)
    sim.reset()

    // provide an instruction
    sim.poke("io_imem_resp_valid", 1)
    sim.poke("io_imem_resp_bits_data", RiscV.symbolicAdd)

    // initialize the register file
    sim.poke("core.dpath.regfile", RiscV.BaseRegs)

    // uncomment for debugging
    // sim.stepAndMonitor(4, List("core.frontend.if_reg_pc", "core.dpath.regfile"))

    // we wait until the register file changes
    while (sim.peek("core.dpath.regfile").toString == "Value(regs)") {
      sim.step()
    }

    val regsNext = sim.peek("core.dpath.regfile")
    RiscV.verifyAdd(regsNext.getSMT.asInstanceOf[smt.ArrayExpr])
  }
}
