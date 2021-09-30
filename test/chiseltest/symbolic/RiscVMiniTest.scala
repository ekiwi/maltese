// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.smt
import maltese.smt.DeclareFunction
import maltese.smt.solvers.{Yices2, Yices2SMTLib, Z3SMTLib}
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
    sim.poke("dpath.regFile.regs", RiscV.BaseRegs)

    // it takes 4 steps until the instruction is executed
    // we wait until the register file changes
    while(sim.peek("dpath.regFile.regs").toString == "Value(regs)") {
      sim.step()
    }

    val regsNext = sim.peek("dpath.regFile.regs")
    RiscV.verifyAdd(regsNext.getSMT.asInstanceOf[smt.ArrayExpr])
  }
}

object RiscV {
  val BaseRegs = smt.ArraySymbol("regs", indexWidth = 5, dataWidth = 32)
  val Rs1 = smt.BVSymbol("rs1", 5)
  val Rs2 = smt.BVSymbol("rs2", 5)
  val Rd = smt.BVSymbol("rd", 5)

  val symbolicAdd = Cat(Seq(smt.BVLiteral(0, 7), Rs2, Rs1, smt.BVLiteral(0, 3), Rd, smt.BVLiteral(51, 7)))
  private def Cat(items: Seq[smt.BVExpr]): smt.BVExpr = {
    require(items.nonEmpty)
    items.drop(1).foldLeft(items.head)(smt.BVConcat)
  }

  private def isX0(addr: smt.BVExpr): smt.BVExpr = smt.BVEqual(addr, smt.BVLiteral(0, 5))
  private def readReg(addr: smt.BVExpr): smt.BVExpr =
    smt.BVIte(isX0(addr), smt.BVLiteral(0, 32), smt.ArrayRead(BaseRegs, addr))
  private def writeReg(addr: smt.BVExpr, value: smt.BVExpr): smt.ArrayExpr =
    smt.ArrayIte(isX0(addr), BaseRegs, smt.ArrayStore(BaseRegs, addr, value))

  lazy val solver = {
    val s = Z3SMTLib.createContext()
    s.setLogic("QF_AUFBV")
    // declare all inputs
    Seq(BaseRegs, Rs1, Rs2, Rd).map(DeclareFunction(_, Seq())).foreach(s.runCommand)
    s
  }

  def verifyAdd(regsNext: smt.ArrayExpr): Unit = {
    val expected = writeReg(Rd, smt.BVOp(smt.Op.Add, readReg(Rs1), readReg(Rs2)))
    // does there exist an assignment that makes the two expressions not equal?
    val formula = smt.BVNot(smt.ArrayEqual(regsNext, expected))
    val r = solver.check(formula)
    assert(r.isUnSat, s"$regsNext\n==//==\n$expected")
  }
}
