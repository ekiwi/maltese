// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.FileUtils
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicSimTest extends AnyFlatSpec {
  behavior of "SymbolicSim"

  it should "be able to load a simple btor file" in {
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

}
