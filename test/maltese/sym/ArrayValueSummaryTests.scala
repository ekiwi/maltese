// Copyright 2019-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.sym

import maltese.smt._

import org.scalatest.flatspec.AnyFlatSpec

class ArrayValueSummaryTests extends AnyFlatSpec {
  behavior.of("ArrayValudSummary")
  implicit val ctx: SymbolicContext = new SymbolicContext(Options.Default)

  it should "recognize that a constant array is concrete" in {
    val a = ArrayConstant(BVLiteral(123, 8), 4)
    val vs = ArrayValueSummary(a)

    assert(vs.isConcrete)

    //  since the array is concrete, we can access each index
    val value = vs.value.get
    (0 until (1 << 4)).foreach { ii =>
      assert(value(ii) == 123)
    }

    // or we can create a dense indexed sequence
    assert(value.toIndexedSeq == IndexedSeq.fill(16)(123))

    // or we can extract the default value + any changes
    assert(value.toMap._1 == 123)
    assert(value.toMap._2.isEmpty)
  }

  it should "recognize that a constant array with a constant store is concrete" in {
    val a = ArrayConstant(BVLiteral(123, 8), 4)
    val vs = ArrayValueSummary.store(ArrayValueSummary(a), BVValueSummary(3, 4), BVValueSummary(100, 8))

    assert(vs.isConcrete)

    //  since the array is concrete, we can access each index
    val value = vs.value.get
    (0 until (1 << 4)).foreach { ii =>
      assert(value(ii) == (if (ii == 3) 100 else 123))
    }

    // or we can create a dense indexed sequence
    assert(
      value.toIndexedSeq == IndexedSeq(123, 123, 123, 100, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123)
    )

    // or we can extract the default value + any changes
    assert(value.toMap._1 == 123)
    assert(value.toMap._2 == Map(3 -> 100))
  }

  it should "return a constant for a store and load (on constant base array)" in {
    val a = ArrayConstant(BVLiteral(123, 8), 4)
    val stored = ArrayValueSummary.store(ArrayValueSummary(a), BVValueSummary(3, 4), BVValueSummary(100, 8))
    val load = BVValueSummary.read(stored, BVValueSummary(3, 4))

    assert(load.isConcrete)
    assert(load.value.get == 100)
  }

  it should "return a constant for a store and load (on symbolic base array)" in {
    val a = ArraySymbol("a", indexWidth = 4, dataWidth = 8)
    val stored = ArrayValueSummary.store(ArrayValueSummary(a), BVValueSummary(3, 4), BVValueSummary(100, 8))
    val load = BVValueSummary.read(stored, BVValueSummary(3, 4))

    assert(load.isConcrete)
    assert(load.value.get == 100)
  }
}
