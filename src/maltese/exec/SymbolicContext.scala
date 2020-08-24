// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.exec

import net.sf.javabdd._
import maltese.smt._
import maltese.smt.solvers._

class SymbolicContext(
   // MultiSE 3.2: without coalescing we get an algorithm that behaves essentially like conventional DSE
   val DoNotCoalesce : Boolean = false,
   // This will slow down symbolic execution significantly, only enable for debugging
   //val CrosscheckSmtAndConcrete : Boolean = false,
   // Do not use the SMT formula cache (this enforces a solver call for every isValid or isUnSat query)
   //val DisableSmtCache: Boolean = false,
   // Encode memory reads as ITEs (in ValueSummary form) instead of using Array theory (Store/Select)
   //val EncodeMemoryReadsAsValueSummary: Boolean = false,
   // call solver to check if memory index could be out of bounds while enable is high
   //val MemoryCheckForOutOfBoundsWriteWithSolver: Boolean = true,
   // use heuristics to simplify [[ValueSummary]]s on the fly (no solver is invoked)
   //val SimplifySymbolicValueSummariesWithHeuristics: Boolean = true,
   // make use of cached isUnSat/isValid results when converting smt expressions to BDDs
   //val QuerySmtCacheInSmtToBdd: Boolean = true,
   // converts boolean ops in the SMT formula to BDD and only assigns literals in other theories to BDD variables
   // (has been observed to reduce the number of entries from 40 -> 28 in one instance)
   val ConvertBooleanOpsInSmtToBdd: Boolean = true,
   // tries to discover relationship between atomic predicates and use them to simplify BDDs
   //val MinePredicateTheoremsInSmtToBdd: Boolean = false,
   // runs a isUnSat query on every guard in the ValueSummary resulting from the ITE, unsat entries are discarded
   val CheckITEConditionWithSmtSolver: Boolean = false,
   // Converts boolean SMT expressions into BDDs and imports them into the guard.
   // This way the value summary will always have max. two entries.
   val ImportBooleanExpressionsIntoGuard: Boolean = true,
   // SMT solver to use
   solver : Solver = Yices2(),
   // BDD implementation
   bdds : BDDFactory = JFactory.init(100, 100)
 ) {

 def isUnSat(bdd: BDD): Boolean = isUnSat(bddConverter.bddToSmt(bdd))

 def isUnSat(expr: BVExpr): Boolean = {
  assert(expr.width == 1, "satisfiability checks require a boolean formula")
  // TODO: add optimizations and caching
  solver.check(expr, false).isUnSat
 }

  ///////////////// BDDs
  private val bddConverter = new BDDToSMTConverter(bdds, ConvertBooleanOpsInSmtToBdd)
  def smtToBdd(ee: BVExpr): BDD = bddConverter.smtToBdd(ee)
  def bddToSmt(bdd: BDD): BVExpr = bddConverter.bddToSmt(bdd)
  val tru: BDD = bddConverter.tru


  def printStatistics(): Unit = {
    println("Atoms: " + bddConverter.getCacheSize)
  }

}
