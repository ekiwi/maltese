// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese

import uclid.smt.Btor2

object MalteseApp extends App {
  if(args.length < 1) {
    println(s"please provide the name of a btor file")
  } else {
    Maltese.load(args.head)
  }
}

object Maltese {
  def load(filename: String): Unit = {
    // load transition system from file
    val sys = Btor2.load(filename)

    println(sys.name.get)
    sys.inputs.foreach(i => println(s"input ${i.id} : ${i.typ}"))
    sys.states.foreach(s => println(s"state ${s.sym.id} : ${s.sym.typ} = [init] ${s.init} [next] ${s.next}"))
    sys.outputs.foreach(o => println(s"output ${o._1} : ${o._2.typ} = ${o._2}"))
    sys.constraints.foreach(c => println(s"assume ${c}"))
    sys.bad.foreach(b => println(s"assert ${b}"))

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
