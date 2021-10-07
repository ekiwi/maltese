package chiseltest.symbolic

import org.scalatest.flatspec.AnyFlatSpec

class BoomTest extends AnyFlatSpec {
  behavior of "SmallBoom"

  // TODO: for now the loading fails :(
  it should "execute a symbolic add" ignore {
    val sim = SymbolicSim.loadFirrtlFile("benchmarks/chipyard/chipyard.TestHarness.SmallBoomConfig/chipyard.TestHarness.SmallBoomConfig.fir")

    // set all inputs to a concrete value and reset design
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()
  }
}
