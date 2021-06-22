// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import org.scalatest.flatspec.AnyFlatSpec

class IodineTest extends AnyFlatSpec {
  behavior of "iodine FPU"

  it should "execute symbolically" in {
    val sim = SymbolicSim.loadBtor("benchmarks/iodine/divider.btor2")

    sim.poke("rst", 1)
    sim.step()
    sim.poke("rst", 0)
    sim.poke("input_a_stb", 1)
    sim.poke("input_b_stb", 1)
    sim.step()
    sim.step()
    sim.step()
    sim.step()
    sim.step()
    sim.poke("output_z_ack", 1)
    sim.poke("input_a_stb", 0)
    sim.poke("input_b_stb", 0)
    sim.step()
    assert(sim.peek("output_z_stb").isConcrete && sim.peek("output_z_stb").getValue == 0)
    sim.step()
    val valid = sim.peek("output_z_stb")
    assert(valid.isSymbolic, "whether or not the output is valid, depends on the input!")
  }
}
