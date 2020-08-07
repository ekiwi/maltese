// Licensed under Apache 2.0 and 3-Clause BSD
// Inspired by the uclid5 SMT library (https://github.com/uclid-org/uclid).
// And the btor2 documentation (BTOR2 , BtorMC and Boolector 3.0 by Niemetz et.al.)

package maltese.smt

sealed trait SMTExpr
sealed trait SMTSymbol extends SMTExpr { val name: String }
object SMTSymbol {
  def fromExpr(name: String, e: SMTExpr): SMTSymbol = e match {
    case b: BVExpr => BVSymbol(name, b.width)
    case a: ArrayExpr => ArraySymbol(name, a.indexWidth, a.dataWidth)
  }
}

sealed trait BVExpr extends SMTExpr { def width: Int }
case class BVLiteral(value: BigInt, width: Int) extends BVExpr {
  private def minWidth = value.bitLength + (if(value <= 0) 1 else 0)
  assert(width > 0, "Zero or negative width literals are not allowed!")
  assert(width >= minWidth, "Value (" + value.toString + ") too big for BitVector of width " + width + " bits.")
  override def toString: String = if(width <= 8) {
    width.toString + "'b" + value.toString(2)
  } else { width.toString + "'x" + value.toString(16) }
}
case class BVSymbol(name: String, width: Int) extends BVExpr with SMTSymbol {
  assert(!name.contains("|"),  s"Invalid id $name contains escape character `|`")
  assert(!name.contains("\\"), s"Invalid id $name contains `\\`")
  assert(width > 0, "Zero width bit vectors are not supported!")
  override def toString: String = name
  def toStringWithType: String = name + " : bv" + width.toString
}

sealed trait UnaryBVExpr extends BVExpr { def e: BVExpr }
case class Extend(e: BVExpr, by: Int, signed: Boolean) extends UnaryBVExpr {
  assert(by >= 0, "Extension must be non-negative!")
  override val width: Int = e.width + by
  override def toString: String = if(signed) { s"sext($e, $by)" } else { s"zext($e, $by)" }
}
// also known as bit extract operation
case class Slice(e: BVExpr, hi: Int, lo: Int) extends UnaryBVExpr {
  assert(lo >= 0, s"lo (lsb) must be non-negative!")
  assert(hi >= lo, s"hi (msb) must not be smaller than lo (lsb): msb: $hi lsb: $lo")
  assert(e.width > hi, s"Out off bounds hi (msb) access: width: ${e.width} msb: $hi")
  override def width: Int = hi - lo + 1
  override def toString: String = if(hi == lo) s"$e[$hi]" else s"$e[$hi:$lo]"
}
case class Not(e: BVExpr) extends UnaryBVExpr {
  override val width: Int = e.width
  override def toString: String = s"not($e)"
}
case class Negate(e: BVExpr) extends UnaryBVExpr {
  override val width: Int = e.width
  override def toString: String = s"neg($e)"
}
case class ReduceOr(e: BVExpr) extends UnaryBVExpr {
  override def width: Int = 1
  override def toString: String = s"redor($e)"
}
case class ReduceAnd(e: BVExpr) extends UnaryBVExpr {
  override def width: Int = 1
  override def toString: String = s"redand($e)"
}
case class ReduceXor(e: BVExpr) extends UnaryBVExpr {
  override def width: Int = 1
  override def toString: String = s"redxor($e)"
}

sealed trait BinaryBVExpr extends BVExpr {
  def a: BVExpr
  def b: BVExpr
}
case class Implies(a: BVExpr, b: BVExpr) extends BinaryBVExpr {
  assert(a.width == 1 && b.width == 1, s"Both arguments need to be 1-bit!")
  override def width: Int = 1
  override def toString: String = s"impl($a, $b)"
}
case class BVEqual(a: BVExpr, b: BVExpr) extends BinaryBVExpr {
  assert(a.width == b.width, s"Both argument need to be the same width!")
  override def width: Int = 1
  override def toString: String = s"eq($a, $b)"
}
object Compare extends Enumeration {
  val Greater, GreaterEqual = Value
}
case class BVComparison(op: Compare.Value, a: BVExpr, b: BVExpr, signed: Boolean) extends BinaryBVExpr {
  assert(a.width == b.width, s"Both argument need to be the same width!")
  override def width: Int = 1
  override def toString: String = op match {
    case Compare.Greater => (if(signed) "sgt" else "ugt") + s"($a, $b)"
    case Compare.GreaterEqual => (if(signed) "sgeq" else "ugeq") + s"($a, $b)"
  }
}
object Op extends Enumeration {
  val And = Value("and")
  val Or = Value("or")
  val Xor = Value("xor")
  val ShiftLeft = Value("logical_shift_left")
  val ArithmeticShiftRight = Value("arithmetic_shift_right")
  val ShiftRight = Value("logical_shift_right")
  val Add = Value("add")
  val Mul = Value("mul")
  val SignedDiv = Value("sdiv")
  val UnsignedDiv = Value("udiv")
  val SignedMod = Value("smod")
  val SignedRem = Value("srem")
  val UnsignedRem = Value("urem")
  val Sub = Value("sub")
}
case class BVOp(op: Op.Value, a: BVExpr, b: BVExpr) extends BinaryBVExpr {
  assert(a.width == b.width, s"Both argument need to be the same width!")
  override val width: Int = a.width
  override def toString: String = s"$op($a, $b)"
}
case class BVConcat(a: BVExpr, b: BVExpr) extends BinaryBVExpr {
  override val width: Int = a.width + b.width
  override def toString: String = s"concat($a, $b)"
}
case class ArrayRead(array: ArrayExpr, index: BVExpr) extends BVExpr {
  assert(array.indexWidth == index.width, "Index with does not match expected array index width!")
  override val width: Int = array.dataWidth
  override def toString: String = s"$array[$index])"
}
case class BitVectorIte(cond: BVExpr, tru: BVExpr, fals: BVExpr) extends BVExpr {
  assert(cond.width == 1, s"Condition needs to be a 1-bit value not ${cond.width}-bit!")
  assert(tru.width == fals.width, s"Both branches need to be of the same width! ${tru.width} vs ${fals.width}")
  override val width: Int = tru.width
  override def toString: String = s"ite($cond, $tru, $fals)"
}

sealed trait ArrayExpr extends SMTExpr { val indexWidth: Int; val dataWidth: Int }
case class ArraySymbol(name: String, indexWidth: Int, dataWidth: Int) extends ArrayExpr with SMTSymbol {
  assert(!name.contains("|"),  s"Invalid id $name contains escape character `|`")
  assert(!name.contains("\\"), s"Invalid id $name contains `\\`")
  override def toString: String = name
  def toStringWithType: String = s"$name : bv<$indexWidth> -> bv<$dataWidth>"
}
case class ArrayStore(array: ArrayExpr, index: BVExpr, data: BVExpr) extends ArrayExpr {
  assert(array.indexWidth == index.width, "Index with does not match expected array index width!")
  assert(array.dataWidth == data.width, "Data with does not match expected array data width!")
  override val dataWidth: Int = array.dataWidth
  override val indexWidth: Int = array.indexWidth
  override def toString: String = s"$array[$index := $data])"
}
case class ArrayIte(cond: BVExpr, tru: ArrayExpr, fals: ArrayExpr) extends ArrayExpr {
  assert(cond.width == 1, s"Condition needs to be a 1-bit value not ${cond.width}-bit!")
  assert(tru.indexWidth == fals.indexWidth,
    s"Both branches need to be of the same type! ${tru.indexWidth} vs ${fals.indexWidth}")
  assert(tru.dataWidth == fals.dataWidth,
    s"Both branches need to be of the same type! ${tru.dataWidth} vs ${fals.dataWidth}")
  override val dataWidth: Int = tru.dataWidth
  override val indexWidth: Int = tru.indexWidth
  override def toString: String = s"ite($cond, $tru, $fals)"
}
case class ArrayEqual(a: ArrayExpr, b: ArrayExpr) extends BVExpr {
  assert(a.indexWidth == b.indexWidth, s"Both argument need to be the same index width!")
  assert(a.dataWidth == b.dataWidth, s"Both argument need to be the same data width!")
  override def width: Int = 1
  override def toString: String = s"eq($a, $b)"
}
case class ConstantArray(e: BVExpr, indexWidth: Int) extends ArrayExpr {
  override val dataWidth: Int = e.width
  override def toString: String = s"([$e] x ${ (BigInt(1) << indexWidth) - 1 })"
}

object SMTEqual {
  def apply(a: SMTExpr, b: SMTExpr): BVExpr = (a,b) match {
    case (ab : BVExpr, bb : BVExpr) => BVEqual(ab, bb)
    case (aa : ArrayExpr, ba: ArrayExpr) => ArrayEqual(aa, ba)
    case _ => throw new RuntimeException(s"Cannot compare $a and $b")
  }
}

object SMTIte {
  def apply(cond: BVExpr, tru: SMTExpr, fals: SMTExpr): SMTExpr = (tru, fals) match {
    case (ab : BVExpr, bb : BVExpr) => BitVectorIte(cond, ab, bb)
    case (aa : ArrayExpr, ba: ArrayExpr) => ArrayIte(cond, aa, ba)
    case _ => throw new RuntimeException(s"Cannot mux $tru and $fals")
  }
}

// minimal function application expressions needed for the SMT Transition System Encoding
private case class BVRawExpr(serialized: String, width: Int) extends BVExpr
private case class ArrayRawExpr(serialized: String, indexWidth: Int, dataWidth: Int) extends ArrayExpr