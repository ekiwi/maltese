package chiseltest.symbolic

import org.scalatest.flatspec.AnyFlatSpec

class ChipyardSodorTests extends AnyFlatSpec {
  behavior.of("Sodor 3 from Chipyard")

  // TODO: deal with extmodules!
  it should "execute a symbolic add" ignore {
    val sim = SymbolicSim.loadFirrtlFile(
      "benchmarks/chipyard/chipyard.TestHarness.Sodor3StageConfig/chipyard.TestHarness.Sodor3StageConfig.fir"
    )

    // set all inputs to a concrete value and reset design
    sim.inputs.foreach(sim.poke(_, 0))
    sim.reset()
  }
}
