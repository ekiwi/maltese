// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.annotations.{
  CircuitTarget,
  MemoryArrayInitAnnotation,
  MemoryFileInlineAnnotation,
  MemoryLoadFileType,
  MemoryScalarInitAnnotation
}
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class MemoryTests extends AnyFlatSpec {
  behavior.of("SymbolicSim w/ Memories")

  private val simpleMem =
    """circuit Test :
      |  module Test :
      |    input clock : Clock
      |    input addr : UInt<5>
      |    input dataIn : UInt<7>
      |    output dataOut : UInt<7>
      |    input writeEn : UInt<1>
      |
      |    mem m :
      |      data-type => UInt<7>
      |      depth => 32
      |      read-latency => 0
      |      write-latency => 1
      |      reader => r
      |      writer => w
      |    m.r.clk <= clock
      |    m.r.en <= UInt<1>(1)
      |    m.r.addr <= addr
      |    dataOut <= m.r.data
      |
      |    m.w.clk <= clock
      |    m.w.en <= UInt<1>(1)
      |    m.w.mask <= writeEn
      |    m.w.addr <= addr
      |    m.w.data <= dataIn
      |
      """.stripMargin
  private val memTarget = CircuitTarget("Test").module("Test").ref("m")

  it should "memory primitives should run this circuit" in {
    val sim = SymbolicSim.loadFirrtl(simpleMem)

    // by default we are reading from a symbolic address
    def data = sim.peek("dataOut")
    assert(data.toString == "Value(m@0[addr@0])")
    assert(data.isSymbolic)

    // now let's read from address 0
    sim.poke("addr", 0)
    assert(data.toString == "Value(m@0[5'b0])")
    assert(data.isSymbolic)

    // write a concrete value to address zero
    sim.poke("addr", 0)
    sim.poke("writeEn", 1)
    sim.poke("dataIn", 123)
    sim.step()

    // now reading from address 0 should return a concrete value
    sim.poke("addr", 0)
    assert(data.isConcrete, f"Data is not concrete: ${data}")
    assert(data.getValue == 123)
  }

  it should "support initializing a memory with a scalar value" in {
    val anno = MemoryScalarInitAnnotation(memTarget, 55)
    val sim = SymbolicSim.loadFirrtl(simpleMem, Seq(anno))
    def data = sim.peek("dataOut")

    // with a concrete address, we should get a concrete value
    sim.poke("addr", 0)
    assert(data.isConcrete)
    assert(data.getValue == 55)

    // with a symbolic address it should be the same, since every address maps to the same value
    sim.pokeDontCare("addr")
    assert(data.isConcrete)
    assert(data.getValue == 55)
  }

  private def testArrayInit(values: Seq[BigInt], sim: SymbolicSim): Unit = {
    def data = sim.peek("dataOut")

    // with a concrete address, we should get a concrete value
    values.zipWithIndex.foreach {
      case (value, addr) =>
        sim.poke("addr", addr)
        assert(data.isConcrete)
        assert(data.getValue == value)
    }

    // with a symbolic address, we get a symbolic value
    sim.pokeDontCare("addr")
    assert(data.isSymbolic)

    // we can overwrite a value
    sim.poke("writeEn", 1)
    sim.poke("addr", 17)
    sim.poke("dataIn", 123)
    sim.step()
    assert(data.isConcrete)
    assert(data.getValue == 123)
  }

  it should "support initializing a memory with an array of values" in {
    val rand = new Random(0)
    val values = Seq.tabulate(32)(_ => BigInt(7, rand))
    val anno = MemoryArrayInitAnnotation(memTarget, values)
    val sim = SymbolicSim.loadFirrtl(simpleMem, Seq(anno))
    testArrayInit(values, sim)
  }

  it should "support initializing memories through a file" in {
    val rand = new Random(0)
    val values = Seq.tabulate(32)(_ => BigInt(7, rand))
    // we write the values to a temporary file in hex format
    val hexValues = values.map(_.toString(16)).mkString("\n") + "\n"
    val hexFile = os.temp(hexValues)
    val anno = MemoryFileInlineAnnotation(memTarget, hexFile.toString(), MemoryLoadFileType.Hex)
    val sim = SymbolicSim.loadFirrtl(simpleMem, Seq(anno))
    testArrayInit(values, sim)
  }

  it should "support poking a memory location" in {
    val sim = SymbolicSim.loadFirrtl(simpleMem)

    // by default we are reading from the initial memory symbol
    sim.poke("addr", 0)
    def data = sim.peek("dataOut")
    assert(data.toString == "Value(m@0[5'b0])")
    assert(data.isSymbolic)

    // after poking a concrete value, we should get that back
    sim.pokeMemory("m", 0, 123)
    assert(data.isConcrete)
    assert(data.getValue == 123)
  }

  it should "support peeking a memory location" in {
    val sim = SymbolicSim.loadFirrtl(simpleMem)

    // by default we are reading from the initial memory symbol
    def data = sim.peekMemory("m", 0)
    assert(data.toString == "Value(m@0[5'b0])")
    assert(data.isSymbolic)

    // after poking a concrete value, we should get that back
    sim.pokeMemory("m", 0, 123)
    assert(data.isConcrete)
    assert(data.getValue == 123)
  }
}
