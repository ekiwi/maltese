package chiseltest.symbolic

import chiseltest.simulator.{Compiler, SimulatorContext, VerilatorBackendAnnotation, WriteVcdAnnotation}
import firrtl.{AnnotationSeq, CircuitState}
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

class ConcreteRocketTest extends FlatSpecWithTargetDir {
  behavior of "RocketTile from Chipyard"

  def loadFirrtl(src: String, annos: AnnotationSeq = List()): CircuitState = {
    val state = CircuitState(firrtl.Parser.parse(src), annos)
    Compiler.toLowFirrtl(state)
  }

  def load(src: String, annos: AnnotationSeq = List()): SimulatorContext = {
    VerilatorBackendAnnotation.getSimulator.createContext(loadFirrtl(src, withTargetDir(annos)))
  }

  it should "execute in a simulation" in {
    val filename = os.pwd / "benchmarks" / "chipyard" / "chipyard.TestHarness.TinyRocketConfig" / "RocketTile.opt.lo.fir"
    val sim = load(os.read(filename), Seq(WriteVcdAnnotation))
    sim.poke("reset", 1)
    sim.step()
    sim.poke("reset", 0)
    sim.step(5)
    sim.finish()
  }

}
