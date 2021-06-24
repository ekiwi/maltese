// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import org.scalatest.flatspec.AnyFlatSpec

class MemoryTests extends AnyFlatSpec {
  behavior of "SymbolicSim w/ Memories"

  it should "memory primitives should run this circuit" in {
    val input =
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

    val sim = SymbolicSim.loadFirrtl(input)

    sim.poke("writeEn", 0)
    val data = sim.peek("dataOut")
    println(s"data: $data")
    sim.step()

  }
}
