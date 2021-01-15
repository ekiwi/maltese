// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

import maltese.mc._
import org.scalatest.flatspec.AnyFlatSpec

class Btor2SerializerSpec extends AnyFlatSpec {
  behavior of "Btor2Serializer"

  private val arraySystem = TransitionSystem(
    "arrays",
    inputs = List(),
    next = List(
      Signal("a0.init", ArrayConstant(BVLiteral(0, 8), 5), IsInit),
      Signal("a0.next", ArrayConstant(BVLiteral(0xff, 8), 5), IsNext)
    ),
    states = List(ArrayState.withInitAndNext("a0", 5, 8))
  )

  it should "serialize a system with arrays" in {
    val expected =
      """array
        |""".stripMargin
    println(arraySystem.serialize)
    val sys = Btor2Serializer.serialize(arraySystem).mkString("\n")
    assert(sys.trim == expected.trim)
  }

}
