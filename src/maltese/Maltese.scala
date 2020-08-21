// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese

import java.io.File

import maltese.exec.Engine
import maltese.passes._
import maltese.smt.IsBad

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

    Simplify,

    PrintSystem,
  )

  def load(filename: String): Unit = {
    // load transition system from file
    val sys = smt.Btor2.load(new File(filename))

    println(s"Loaded $filename")

    val simplified = PassManager(passes).run(sys, trace = true)

    val e = Engine(simplified)

    val bad = simplified.signals.filter(_.lbl == IsBad).map(_.name)
    bad.foreach { b =>
      (0 until 3).foreach { step =>
        val r = e.signalAt(b, step)
        println(s"$b@$step: $r")
      }
    }
  }
}
