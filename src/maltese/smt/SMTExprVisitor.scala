// Licensed under Apache 2.0 and 3-Clause BSD
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

/** Similar to the mapExpr and foreachExpr methods of the firrtl ir nodes, but external to the case classes */
object SMTExprVisitor {
  type ArrayFun = ArrayExpr => ArrayExpr
  type BVFun = BVExpr => BVExpr

  def map[T <: SMTExpr](bv: BVFun, ar: ArrayFun)(e: T): T = e match {
    case b: BVExpr => map(b, bv, ar).asInstanceOf[T]
    case a: ArrayExpr => map(a, bv, ar).asInstanceOf[T]
  }
  def map[T <: SMTExpr](f: SMTExpr => SMTExpr)(e: T): T =
    map(b => f(b).asInstanceOf[BVExpr], a => f(a).asInstanceOf[ArrayExpr])(e)

  private def map(e: BVExpr, bv: BVFun, ar: ArrayFun): BVExpr = e match {
    // nullary
    case old : BVLiteral => bv(old)
    case old : BVSymbol => bv(old)
    case old : BVRawExpr => bv(old)
    // unary
    case old @ Extend(e, by, signed) => val n = map(e, bv, ar) ; bv(if(n.eq(e)) old else Extend(n, by, signed))
    case old @ Slice(e, hi, lo) => val n = map(e, bv, ar) ; bv(if(n.eq(e)) old else Slice(n, hi, lo))
    case old @ Not(e) => val n = map(e, bv, ar) ; bv(if(n.eq(e)) old else Not(n))
    case old @ Negate(e) => val n = map(e, bv, ar) ; bv(if(n.eq(e)) old else Negate(n))
    case old @ ReduceAnd(e) => val n = map(e, bv, ar) ; bv(if(n.eq(e)) old else ReduceAnd(n))
    case old @ ReduceOr(e) => val n = map(e, bv, ar) ; bv(if(n.eq(e)) old else ReduceOr(n))
    case old @ ReduceXor(e) => val n = map(e, bv, ar) ; bv(if(n.eq(e)) old else ReduceXor(n))
    // binary
    case old @ Implies(a, b) =>
      val (nA, nB) = (map(a, bv, ar), map(b, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b)) old else Implies(nA, nB))
    case old @ BVEqual(a, b) =>
      val (nA, nB) = (map(a, bv, ar), map(b, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b)) old else BVEqual(nA, nB))
    case old @ ArrayEqual(a, b) =>
      val (nA, nB) = (map(a, bv, ar), map(b, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b)) old else ArrayEqual(nA, nB))
    case old @ BVComparison(op, a, b, signed) =>
      val (nA, nB) = (map(a, bv, ar), map(b, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b)) old else BVComparison(op, nA, nB, signed))
    case old @ BVOp(op, a, b) =>
      val (nA, nB) = (map(a, bv, ar), map(b, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b)) old else BVOp(op, nA, nB))
    case old @ BVConcat(a, b) =>
      val (nA, nB) = (map(a, bv, ar), map(b, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b)) old else BVConcat(nA, nB))
    case old @ ArrayRead(a, b) =>
      val (nA, nB) = (map(a, bv, ar), map(b, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b)) old else ArrayRead(nA, nB))
    // ternary
    case old @ BitVectorIte(a, b, c) =>
      val (nA, nB, nC) = (map(a, bv, ar), map(b, bv, ar), map(c, bv, ar))
      bv(if(nA.eq(a) && nB.eq(b) && nC.eq(c)) old else BitVectorIte(nA, nB, nC))
  }


  private def map(e: ArrayExpr, bv: BVFun, ar: ArrayFun): ArrayExpr = e match {
    case old : ArrayRawExpr => ar(old)
    case old : ArraySymbol => ar(old)
    case old @ ConstantArray(e, indexWidth) =>
      val n = map(e, bv, ar) ; ar(if(n.eq(e)) old else ConstantArray(n, indexWidth))
    case old @ ArrayStore(a, b, c) =>
      val (nA, nB, nC) = (map(a, bv, ar), map(b, bv, ar), map(c, bv, ar))
      ar(if(nA.eq(a) && nB.eq(b) && nC.eq(c)) old else ArrayStore(nA, nB, nC))
    case old @ ArrayIte(a, b, c) =>
      val (nA, nB, nC) = (map(a, bv, ar), map(b, bv, ar), map(c, bv, ar))
      ar(if(nA.eq(a) && nB.eq(b) && nC.eq(c)) old else ArrayIte(nA, nB, nC))
  }

}
