// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese

object MalteseApp extends App {
  if(args.length < 1) {
    println(s"please provide the name of a btor file")
  } else {
    Maltese.load(args.head)
  }
}

object Maltese {
  def load(filename: String): Unit = ???
}
