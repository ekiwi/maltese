// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.FileUtils
import firrtl.annotations.{CircuitTarget, PresetRegAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

class SimpleFirrtlTests extends AnyFlatSpec {
  behavior of "SymbolicSim with simple firrtl circuits"


  private val count2 =
    """circuit count2:
      |  module count2:
      |    input clock: Clock
      |    reg a : UInt<3>, clock with :
      |      reset => (UInt(0), UInt(0))
      |    a <= add(a, UInt(1))
      |    assert(clock, not(eq(a, UInt(7))), UInt(1), "") : a_not_7
      |""".stripMargin
  private val count2Annos = Seq(PresetRegAnnotation(CircuitTarget("count2").module("count2").ref("a")))

  it should "be able to execute the simple count2 circuit" in {
    val sim = SymbolicSim.loadFirrtl(count2, count2Annos)

    // count2 starts at 0, increments by 1 every cycle, should fail at 7
    (0 until 6).foreach { _ =>
      sim.step()
    }
    val e = intercept[RuntimeException] {
      sim.step()
    }
    assert(e.toString.contains("cycle 7"))
  }


  private val twocount2 =
    """circuit twocount2:
      |  module twocount2:
      |    input clock: Clock
      |    input reset: UInt<1>
      |    input turn: UInt<1>
      |
      |    reg a : UInt<2>, clock with :
      |      reset => (reset, UInt(0))
      |    reg b : UInt<2>, clock with :
      |      reset => (reset, UInt(0))
      |    when turn :
      |      b <= add(b, UInt(1))
      |    else :
      |      a <= add(a, UInt(1))
      |""".stripMargin
}