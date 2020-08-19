// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese

import java.io.File

import maltese.passes._
import smt.TransitionSystem


object MalteseApp extends App {
  if(args.length < 1) {
    println(s"please provide the name of a btor file")
  } else {
    Maltese.load(args.head)
  }
}

object Maltese {
  private val passes: Iterable[Pass] = Seq(
    Simplify,
    Inline,
    DeadCodeElimination,

    Simplify,
    Inline,
    DeadCodeElimination,

    // TODO: debug why e.g. "ite(s583, _input_20, ite(1'b0, 2'b0, s578)" is not simplified (should be s578)
    Simplify,
    PrintSystem,
  )

  def load(filename: String): Unit = {
    // load transition system from file
    val sys = smt.Btor2.load(new File(filename))

    println(s"Loaded $filename")

    PassManager(passes).run(sys, trace = true)

    // build model checking context

    // do we start with an arbitrary state or a defined state?
    // i.e. ignore init state?
    // we need to find all the dependencies of each formula and encode them into indexes
    // every signal gets a numeric (Int) index
    // negative index means one step in the past
    // Thus something like a := x + 1 where x is a state variable would have a negative dependency
    // dependencies are tracked in order to know which cell(s) need to be evaluated before the current cell
    // question: how do we deal with "dynamic" dependencies? e.g., mux(cond, a, b)
    // if we know that cond is false, we only need to compute b, not a...

  }
}
