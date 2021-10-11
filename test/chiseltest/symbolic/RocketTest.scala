package chiseltest.symbolic

import org.scalatest.flatspec.AnyFlatSpec

class RocketTest extends AnyFlatSpec {
  behavior of "RocketTile from Chipyard"

  it should "execute a symbolic add" in {
    val sim = SymbolicSim.loadFirrtlFile("benchmarks/chipyard/chipyard.TestHarness.RocketConfig/RocketTile.fir")

    // set all inputs to a concrete value and reset design
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()
  }
}
