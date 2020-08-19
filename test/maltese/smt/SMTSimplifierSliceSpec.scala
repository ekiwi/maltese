// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

/** Slice simplification gets its own spec since it is so important
 *  for recovering boolean operations out of word level operations.
 * */
class SMTSimplifierSliceSpec extends SMTSimplifierBaseSpec {
  behavior of "SMTSimplifier"

  it should "simplify slice no-op" in {
    assert(simplify(BVSlice(bv("a", 3), 2, 0))   == bv("a", 3))
    assert(simplify(BVSlice(bv("a", 13), 12, 0)) == bv("a", 13))
  }

  it should "simplify slice of slice" in {
    assert(simplify(BVSlice(BVSlice(bv("a", 3), 2,1), 1, 1)) ==
      BVSlice(bv("a", 3), 2,2))
    assert(simplify(BVSlice(BVSlice(BVSlice(bv("a", 5), 4,1), 3, 1), 2, 2)) ==
      BVSlice(bv("a", 5), 4,4))
  }
}
