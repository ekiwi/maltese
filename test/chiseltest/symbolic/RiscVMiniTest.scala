// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import maltese.smt._
import maltese.smt.solvers.{Yices2, Yices2SMTLib, Z3SMTLib}
import org.scalatest.flatspec.AnyFlatSpec

class RiscVMiniTest extends AnyFlatSpec {
  behavior of "risc-v mini core"

  it should "execute a symbolic add" in {
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
    RiscV.verifyAdd(regsNext.getSMT.asInstanceOf[ArrayExpr])
  }

  it should "execute a symbolic store" in {
    val sim = SymbolicSim.loadFirrtlFile("benchmarks/riscv/riscv-mini/CoreWithFakeDCache.fir")

    // set all inputs to a concrete value and reset design
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()

    // init mem
    sim.poke("c.mem", RiscV.Mem)
    // initialize the register file
    sim.poke("dpath.regFile.regs", RiscV.BaseRegs)


    val monitorSignals = Seq(
      "io_icache_req_valid", "c_io_dcache_req_bits_mask", "c.mem", "io_icache_req_valid"
    )

    // provide a nop
    sim.poke("io_icache_resp_valid", 1)
    sim.poke("io_icache_resp_bits_data", RiscV.nop)
    sim.stepAndMonitor(1, monitorSignals)

    // provide our symbolic instruction
    sim.poke("io_icache_resp_valid", 1)
    sim.poke("io_icache_resp_bits_data", RiscV.symbolicStoreWord)
    assert(sim.peek("io_icache_req_valid").getValue == 1)
    sim.stepAndMonitor(1, monitorSignals)

    // from now on only nops
    sim.poke("io_icache_resp_bits_data", RiscV.nop)

    // we wait until the memory changes
    while(sim.peek("c.mem").toString == "Value(mem)") {
      sim.stepAndMonitor(1, monitorSignals)
    }

    val memNext = sim.peek("c.mem")
    // TODO: actually check result!
    if(false) {
      println()
      println("Result:")
      println(memNext.getSMT)
    }
  }
}

object RiscV {
  val BaseRegs = ArraySymbol("regs", indexWidth = 5, dataWidth = 32)
  val Rs1 = BVSymbol("rs1", 5)
  val Rs2 = BVSymbol("rs2", 5)
  val Rd = BVSymbol("rd", 5)
  val Imm = BVSymbol("imm", 12)
  // byte based main memory
  val Mem = ArraySymbol("mem", indexWidth = 32 + 2, dataWidth = 8)

  val symbolicAdd = Cat(Seq(BVLiteral(0, 7), Rs2, Rs1, BVLiteral(0, 3), Rd, BVLiteral(51, 7)))
  private def Cat(items: Seq[BVExpr]): BVExpr = {
    require(items.nonEmpty)
    items.drop(1).foldLeft(items.head)(BVConcat)
  }

  // nop is addi x0, x0, 0
  val nop = BVLiteral(BigInt("0010011", 2), 32)

  val symbolicStoreWord = Cat(Seq(BVSlice(Imm, 11,5), Rs2, Rs1, BVLiteral("b010"), BVSlice(Imm, 4, 0), BVLiteral("b0100011")))

  private def isX0(addr: BVExpr): BVExpr = BVEqual(addr, BVLiteral(0, 5))
  private def readReg(addr: BVExpr): BVExpr =
    BVIte(isX0(addr), BVLiteral(0, 32), ArrayRead(BaseRegs, addr))
  private def writeReg(addr: BVExpr, value: BVExpr): ArrayExpr =
    ArrayIte(isX0(addr), BaseRegs, ArrayStore(BaseRegs, addr, value))

  lazy val solver = {
    val s = Z3SMTLib.createContext()
    s.setLogic("QF_AUFBV")
    // declare all inputs
    Seq(BaseRegs, Rs1, Rs2, Rd).map(DeclareFunction(_, Seq())).foreach(s.runCommand)
    s
  }

  def verifyAdd(regsNext: ArrayExpr): Unit = {
    val expected = writeReg(Rd, BVOp(Op.Add, readReg(Rs1), readReg(Rs2)))
    // does there exist an assignment that makes the two expressions not equal?
    val formula = BVNot(ArrayEqual(regsNext, expected))
    val r = solver.check(formula)
    assert(r.isUnSat, s"$regsNext\n==//==\n$expected")
  }
}
