// Copyright 2019-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.sym

import maltese.smt._
import org.scalatest.flatspec.AnyFlatSpec

class MalteseArrayTests extends AnyFlatSpec {
  def addr(i: Int) = BVLiteral(i, 3)

  it should "work with concrete addresses and concrete data" in {
    var mem = MalteseArray("mem", dataWidth = 4, indexWidth = 3)

    // after writing a value with concrete address, the memory should still be a [[ConcreteAddrMemory]]
    mem = mem.store(addr(0), BVLiteral(2, 4))
    assert(mem.hasConcreteIndices)
    // we wrote 2 to address zero
    assert(mem.load(addr(0)).isInstanceOf[BVLiteral])
    assert(mem.load(addr(0)).asInstanceOf[BVLiteral].value == 2)
    // reading from any other address should just return the memory symbol
    assert(mem.load(addr(1)).isInstanceOf[ArrayRead])

    // writing a second value to a different address
    mem = mem.store(addr(1), BVLiteral(5, 4))
    assert(mem.hasConcreteIndices)
    // we wrote 5 to address 1
    assert(mem.load(addr(1)).isInstanceOf[BVLiteral])
    assert(mem.load(addr(1)).asInstanceOf[BVLiteral].value == 5)
    // reading from any other address should just return the memory symbol
    assert(mem.load(addr(2)).isInstanceOf[ArrayRead])

    // we should be able to override a value
    mem = mem.store(addr(0), BVLiteral(7, 4))
    assert(mem.load(addr(0)).asInstanceOf[BVLiteral].value == 7)
  }

  it should "work with concrete addresses and symbolic data" in {
    val a = BVSymbol("a", 4)

    var mem = MalteseArray("mem", dataWidth = 4, indexWidth = 3)

    // an empty memory should just be an array symbol
    val memSymbol = ArraySymbol("mem", dataWidth = 4, indexWidth = 3)
    assert(mem.symbolic == memSymbol)

    // reading from memory should return a select of the address
    val addrSym = BVSymbol("addr", 3)
    assert(mem.load(addrSym) == ArrayRead(memSymbol, addrSym))

    // after writing a value with concrete address, the memory should still be a [[ConcreteAddrMemory]]
    mem = mem.store(addr(0), a)
    assert(mem.hasConcreteIndices)
    // we wrote a to address zero
    assert(!mem.load(addr(0)).isInstanceOf[BVLiteral])
    assert(mem.load(addr(0)) == a)
    // the memory expression should contain the store to address 0
    val new_array_symbol = ArrayStore(memSymbol, addr(0), a)
    assert(mem.symbolic == new_array_symbol)

    // reading from any other address should return the memory symbol
    assert(mem.load(addr(1)) == ArrayRead(memSymbol, addr(1)))

    // insert to a symbolic address
    mem = mem.store(addrSym, a)
    assert(!mem.hasConcreteIndices)
  }

  "ConcreteAddrMemory" should "convert to a SymbolicAddrMemory without loosing any content" ignore {
    val (a, b) = (BVSymbol("a", 4), BVSymbol("b", 4))
    val addrSym = BVSymbol("addr", 3)

    var mem = MalteseArray("mem", dataWidth = 4, indexWidth = 3)
    mem = mem.store(addr(0), a)
    // at this point the memory only contains concrete addresses
    assert(mem.hasConcreteIndices)
    // the next store makes it symbolic
    mem = mem.store(addrSym, b)
    assert(!mem.hasConcreteIndices)

    // the smt expression should contain both stores
    val memSymbol = ArraySymbol("mem", dataWidth = 4, indexWidth = 3)
    val array_0 = ArrayStore(memSymbol, addr(0), a)
    val array_1 = ArrayStore(array_0, addrSym, b)
    assert(mem.symbolic == array_1)

    // reading from address 0 should include both stores
    assert(mem.load(addr(0)) == ArrayRead(array_1, addr(0)))
    // reading from any other address should only include the store to a symbolic address
    val array_2 = ArrayStore(memSymbol, addrSym, b)
    (1 until 8).foreach{ i =>
      assert(mem.load(addr(i)) == ArrayRead(array_2, addr(i)))
    }
  }
}
