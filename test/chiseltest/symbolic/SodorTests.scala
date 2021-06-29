// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.smt
import org.scalatest.flatspec.AnyFlatSpec

class Sodor1StageTests extends AnyFlatSpec {
  behavior of "unpipelined sodor"

  it should "execute symbolically" in {
    val sim = SymbolicSim.loadFirrtlFile("benchmarks/rfuzz/Sodor1Stage.fir")

    println(sim.peek("core.d.regfile").toString)
    println(sim.peek("core.d.pc_reg").toString)
    // set all inputs to a concrete value
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()
    println(sim.peek("core.d.regfile").toString)
    println(sim.peek("core.d.pc_reg").toString)
    // sim.poke("io_imem_req_ready", 1)
    sim.poke("io_imem_resp_valid", 1)
    sim.poke("io_imem_resp_bits_data", RiscV.symbolicAdd)
    sim.step()
    println(sim.peek("core.d.regfile").toString)
    println(sim.peek("core.d.pc_reg").toString)
    sim.step()
    println(sim.peek("core.d.regfile").toString)
    println(sim.peek("core.d.pc_reg").toString)
  }



}