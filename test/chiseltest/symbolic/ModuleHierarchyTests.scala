package chiseltest.symbolic

import org.scalatest.flatspec.AnyFlatSpec

class ModuleHierarchyTests extends AnyFlatSpec {
  behavior of "SymbolicSim w/ module hierarchies"

  val oneLevel =
    """circuit test:
      |  module child:
      |    input clock: Clock
      |    input in: UInt<7>
      |    output out: UInt<7>
      |    reg r : UInt<7>, clock with :
      |      reset => (UInt(0), UInt(0))
      |    r <= in
      |    out <= r
      |  module test:
      |    input clock: Clock
      |    input in: UInt<7>
      |    output out: UInt<7>
      |    inst c of child
      |    c.clock <= clock
      |    c.in <= in
      |    out <= c.out
      |""".stripMargin

  it should "be able to peek and poke a register in a submodule" in {
    val sim = SymbolicSim.loadFirrtl(oneLevel)

    // test the register
    assert(sim.peek("out").isSymbolic)
    sim.poke("in", 123)
    sim.step()
    assert(sim.peek("out").isConcrete)
    assert(sim.peek("out").getValue == 123)

    // peek the register
    assert(sim.peek("c.r").getValue == 123)

    // poke the register
    sim.poke("c.r", 0)
    assert(sim.peek("out").getValue == 0)
  }

}
