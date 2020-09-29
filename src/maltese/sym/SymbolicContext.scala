// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.sym

import net.sf.javabdd.BDD
import maltese.smt._

class SymbolicContext(val opts: Options) {
  private val solver = opts.makeSolver()

  def isUnSat(bdd: BDD): Boolean = isUnSat(bddConverter.bddToSmt(bdd))
  def isUnSat(expr: BVExpr): Boolean = {
    assert(expr.width == 1, "satisfiability checks require a boolean formula")
    // TODO: add optimizations and caching
    solver.check(expr, false).isUnSat
  }

  ///////////////// BDDs
  private val bddConverter = new BDDToSMTConverter(opts.makeBdds(), opts.ConvertBooleanOpsInSmtToBdd)
  def smtToBdd(ee: BVExpr): BDD = bddConverter.smtToBdd(ee)
  def bddToSmt(bdd: BDD): BVExpr = bddConverter.bddToSmt(bdd)
  val tru: BDD = bddConverter.tru

  def printStatistics(): Unit = {
    println("Atoms: " + bddConverter.getCacheSize)
  }

}
