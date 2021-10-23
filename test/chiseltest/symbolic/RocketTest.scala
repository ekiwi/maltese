package chiseltest.symbolic

import chiseltest.WriteFstAnnotation
import chiseltest.internal.CachingAnnotation
import chiseltest.simulator.{Compiler, SimulatorContext, VerilatorBackendAnnotation, WriteVcdAnnotation}
import firrtl.{AnnotationSeq, CircuitState}
import firrtl.annotations._
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

  val c = CircuitTarget("RocketTile")
  val iCacheMem = c.module(c.name).instOf("frontend", "Frontend").instOf("icache", "ICache").ref("data_arrays_0_0")
  val instructions = Seq(
    BigInt("001080B3", 16), // add x1, x1, x1
    BigInt("00108133", 16), // add x2, x1, x1
    BigInt("001081B3", 16), // add x3, x1, x1
    BigInt("00108233", 16), // add x4, x1, x1
  )
  val initMemAnno = MemoryArrayInitAnnotation(iCacheMem, instructions ++ Seq.fill(1024 - instructions.size)(BigInt(0)))


  it should "execute in a simulation" in {
    val filename = os.pwd / "benchmarks" / "chipyard" / "chipyard.TestHarness.KevinRocketConfig" / "RocketTile.opt.lo.fir"
    val sim = load(os.read(filename), Seq(WriteFstAnnotation, CachingAnnotation))

    val startAddress = 0x10040
    sim.poke("auto_reset_vector_in", startAddress)

    sim.poke("reset", 1)
    sim.step(20)
    sim.poke("reset", 0)

    val server = new TileLinkServer("auto_tl_other_masters_out_", sim)

    sim.step()

    // start processor through interrupt?
    sim.poke("auto_int_local_in_1_1", 1)

    (0 until 100000).foreach { _ =>
      server.run()
      sim.step()
    }
    sim.finish()
  }

}

case class AData(opcode: Int, param: Int, size: Int, source: Int, mask: Int, address: BigInt, data: BigInt)

class TileLinkServer(prefix: String, sim: SimulatorContext) {
  val aReady = prefix + "a_ready"
  val aValid = prefix + "a_valid"
  val eReady = prefix + "e_ready"
  val eValid = prefix + "e_valid"
  val cReady = prefix + "c_ready"
  val cValid = prefix + "c_valid"
  def aFired = sim.peek(aReady) == 1 && sim.peek(aValid) == 1
  def aPacket: Option[AData] = if(!aFired) None else Some(AData(
    opcode = sim.peek(prefix + "a_bits_opcode").toInt,
    param = sim.peek(prefix + "a_bits_param").toInt,
    size = sim.peek(prefix + "a_bits_size").toInt,
    source = sim.peek(prefix + "a_bits_source").toInt,
    mask = sim.peek(prefix + "a_bits_mask").toInt,
    address = sim.peek(prefix + "a_bits_address"),
    data = sim.peek(prefix + "a_bits_data"),
  ))

  // we are always ready to accept requests on the A-Channel
  sim.poke(aReady, 1)
  sim.poke(cReady, 1)
  sim.poke(eReady, 1)

  // called once every step
  def run(): Unit = {
    aPacket match {
      case Some(data) =>
        println(s"A: $data")
      case None => // do nothing
    }
  }
}
