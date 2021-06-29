// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.smt
import org.scalatest.flatspec.AnyFlatSpec

class RiscVMiniTest extends AnyFlatSpec {
  behavior of "risc-v mini core"

  it should "execute symbolically" in {
    val sim = SymbolicSim.loadFirrtlFile("benchmarks/riscv/riscv-mini/Core.fir")

    // set all inputs to a concrete value and reset design
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()

    // the processor stalls if d/icache are not valid
    sim.poke("io_icache_resp_valid", 1)
    sim.poke("io_dcache_resp_valid", 1)

    // provide an instruction
    sim.poke("io_icache_resp_bits_data", RiscV.symbolicAdd)

    // initialize the register file
    val regs = smt.ArraySymbol("regs", indexWidth = 5, dataWidth = 32)
    sim.poke("dpath.regFile.regs", regs)

    // it takes 4 steps until the instruction is executed
    // we wait until the register file changes
    while(sim.peek("dpath.regFile.regs").toString == "Value(regs)") {
      sim.step()
    }

    val regsNext = sim.peek("dpath.regFile.regs")
    // make sure that it at least contains the common case for when all rs1 and rs2 and rd are not zero
    assert(regsNext.toString.contains("regs[rd := sext(concat(1'b0, add(zext(zext(regs[rs1], 1)[31:0], 1), zext(regs[rs2], 1))[31:0]), 1)[31:0]]"))
  }
}

object RiscV {
  val symbolicAdd = Cat(Seq(smt.BVLiteral(0, 7), smt.BVSymbol("rs2", 5), smt.BVSymbol("rs1", 5), smt.BVLiteral(0, 3), smt.BVSymbol("rd", 5), smt.BVLiteral(51, 7)))
  private def Cat(items: Seq[smt.BVExpr]): smt.BVExpr = {
    require(items.nonEmpty)
    items.drop(1).foldLeft(items.head)(smt.BVConcat)
  }
}
