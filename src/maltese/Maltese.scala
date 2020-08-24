// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese

import java.io.File

import maltese.exec.{Engine, Options}
import maltese.passes._
import maltese.smt.{IsBad, IsConstraint, TransitionSystem}

object MalteseApp extends App {
  if(args.length < 1) {
    // println(s"please provide the name of a btor file")
    //val d = "benchmarks/hwmcc19/bv/goel/crafted/cal10.btor2"
    val d = "benchmarks/hwmcc19/bv/wolf/2019A/picorv32_mutBX_nomem-p0.btor"
    Maltese.load(d)
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

    val simplified = simplifySystem(sys)

    getConstraints(simplified, doInit = false)

    //check(simplified)
    //check(sys)


  }

  def simplifySystem(sys: TransitionSystem): TransitionSystem = PassManager(passes).run(sys, trace = true)

  def check(sys: TransitionSystem): Unit = {
    val e = Engine(sys, noInit = true)

    val bad = sys.signals.filter(_.lbl == IsBad).map(_.name)
    bad.foreach { b =>
      (0 until 1).foreach { step =>
        val r = e.signalAt(b, step)
        println(s"$b@$step: $r")
      }
    }
  }


  def getConstraints(sys: TransitionSystem, doInit: Boolean): Unit = {
    val opts = Options.Default.copy(ImportBooleanExpressionsIntoGuard = false)
    val e = Engine(sys, !doInit, opts)
    val const = sys.signals.filter(_.lbl == IsConstraint).map(_.name)
    const.take(2).foreach { c =>
      val step = 0
      val r = e.signalAt(c, step)
      val rString = r.toString
      if(rString.length < 200) {
        println(s"$c@$step: $rString")
      } else {
        println(s"skipping $c@$step because the resulting string is too big")
      }
    }
    e.printStatistics()

  }
}
