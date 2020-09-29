// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese

import java.io.File

import maltese.exec._
import maltese.passes._
import maltese.smt._

object MalteseApp extends App {
  if(args.length < 1) {
    // println(s"please provide the name of a btor file")

    val d = "benchmarks/hwmcc19/bv/wolf/2018A/zipcpu-busdelay-p18.btor" // 02, 18, 24, 30, 36, 43, 46, 47
    //val d = "benchmarks/hwmcc19/bv/goel/crafted/toy_lock_4.btor2"
    //val d = "benchmarks/hwmcc19/bv/goel/crafted/cal10.btor2"
    //val d = "benchmarks/hwmcc19/bv/wolf/2019A/picorv32_mutBX_nomem-p0.btor"
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

    // PrintSystem,
  )

  def load(filename: String): Unit = {
    // load transition system from file
    val sys = smt.Btor2.load(new File(filename))

    println(s"Loaded $filename")

    val simplified = simplifySystem(sys)

    val e0 = getConstraints(simplified, doInit = false)
    val e = check(simplified, kMax = 3, doInit = false)
    //val e = check(sys)

    println()
    e.printStatistics()
  }

  def simplifySystem(sys: TransitionSystem): TransitionSystem = PassManager(passes).run(sys, trace = true)

  def check(sys: TransitionSystem, kMax: Int = 2, doInit: Boolean = true): Engine = {
    val e = Engine(sys, noInit = !doInit)

    val bad = sys.signals.filter(_.lbl == IsBad).map(_.name)
    bad.foreach { b =>
      (0 to kMax).foreach { step =>
        val r = e.signalAt(b, step)
        println(s"$b@$step: $r")
      }
    }

    e
  }


  def getConstraints(sys: TransitionSystem, doInit: Boolean): Engine = {
    val opts = Options.Default.copy(ImportBooleanExpressionsIntoGuard = true)
    val e = Engine(sys, !doInit, opts)
    val const = sys.signals.filter(_.lbl == IsConstraint).map(_.name)
    val values = const.take(105).zipWithIndex.map { case (c, i) =>
      //print(s"$i.")
      val step = 1
      val r = e.signalAt(c, step)
      val rString = r.toString
      if(rString.length < 1000) {
        println(s"$c@$step: $rString")
      } else {
        println(s"skipping $c@$step because the resulting string is too big")
      }
      r
    }

    // this seems to blow up
    // val all = values.reduce((a, b) => BVValueSummary.binary(a, b, BVOp(Op.And, _, _)))
    // println(s"all: $all")

    e
  }
}
