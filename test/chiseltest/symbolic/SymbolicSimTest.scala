// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.FileUtils
import org.scalatest.flatspec.AnyFlatSpec

class SymbolicSimTest extends AnyFlatSpec {
  behavior of "SymbolicSim"

  it should "be able to load a simple btor file" in {
    val src = FileUtils.getTextResource("/btormc/count2.btor")
    val sim = SymbolicSim.loadBtor(src, "count2")
  }

}
