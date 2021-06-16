// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.FileUtils
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicSimTest extends AnyFlatSpec {
  behavior of "SymbolicSim"

  it should "be able to execute the simple count2 example" in {
    val src = FileUtils.getTextResource("/btormc/count2.btor2")
    val sim = SymbolicSim.loadBtor(src, "count2")

    // count2 starts at 0, increments by 1 every cycle, should fail at 7
    (0 until 6).foreach { _ =>
      sim.step()
    }
    val e = intercept[RuntimeException] {
      sim.step()
    }
    assert(e.toString.contains("cycle 7"))
  }


  it should "be able to execute the twocount2 example with concrete inputs" in {
    val src = FileUtils.getTextResource("/btormc/twocount2.btor2")
    val sim = SymbolicSim.loadBtor(src, "twocount2")

    // a and b start at 0
    assert(sim.peek("a").getValue == 0)
    assert(sim.peek("b").getValue == 0)

    // if the "turn" input is true, b is incremented, if !turn, a is incremented
    // the system fails if both a and b reach 3 in the same state

    // increment b to three
    sim.poke("turn", 1)
    assert(sim.peek("b").getValue == 0)
    sim.step()
    assert(sim.peek("b").getValue == 1)
    sim.step()
    assert(sim.peek("b").getValue == 2)
    sim.step()
    assert(sim.peek("b").getValue == 3)

    // increment a to three
    sim.poke("turn", 0)
    assert(sim.peek("a").getValue == 0)
    sim.step()
    assert(sim.peek("a").getValue == 1)
    sim.step()
    assert(sim.peek("a").getValue == 2)

    val e = intercept[RuntimeException] {
      sim.step()
    }
    assert(e.toString.contains("cycle 6"))

    assert(sim.peek("a").getValue == 3)
  }
}
