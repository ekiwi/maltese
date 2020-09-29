// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.exec

import maltese.smt._
import net.sf.javabdd.BDD

class TheoremDataBase(bddConverter: BDDToSMTConverter) {
  case class Theorem(bdd: BDD, expr: BVExpr)


  def add(a: BVValueSummary): Unit = {
    assert(a.width == 1)

  }

  private def getSupport(bdd: BDD): List[Int] = {
    var b = try {
      bdd.support()
    } catch {
      case _: NullPointerException => throw new RuntimeException("something wrong with the support!")
    }
    var vars = List[Int]()
    while(!b.isOne) {
      vars = vars :+ b.`var`()
      b = b.high()
    }
    vars
  }


}
