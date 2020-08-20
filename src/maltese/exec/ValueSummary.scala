// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.exec

import maltese.smt._
import net.sf.javabdd.{BDD, JFactory}

object BVValueSummary {
  def ite(cond: BVValueSummary, tru: => BVValueSummary, fals: => BVValueSummary): BVValueSummary = {
    // if the condition is concrete, we only need to evaluate one side of the mux
    if(cond.isTrue) { tru
    } else if(cond.isFalse) { fals
    } else {

    }

  }
}


case class BVValueSummary(entries: List[BVEntry]) {
  assert(entries.nonEmpty)
  val width = entries.head.width
  assert(entries.forall(_.width == width))
  def isConcrete: Boolean = entries.size == 1 && entries.head.isConcrete
  def concrete: BigInt = { assert(isConcrete) ; entries.head.asInstanceOf[BVConcreteEntry].value }
  def isTrue: Boolean = isConcrete && concrete == 1
  def isFalse: Boolean = isConcrete && concrete == 0
}

trait BVEntry {
  def guard: BDD
  def isConcrete: Boolean
  def width: Int
}
case class BVConcreteEntry(guard: BDD, value: BigInt, width: Int) extends BVEntry {
  override def isConcrete = true
}
case class BVSymbolicEntry(guard: BDD, value: BVExpr) extends BVEntry {
  override def isConcrete = false
  override def width = value.width
}