package maltese.smt

object Reduce {
  def and(e: BVExpr): BVExpr = {
    if (e.width == 1) { e } else {
      val allOnes = (BigInt(1) << e.width) - 1
      BVEqual(e, BVLiteral(allOnes, e.width))
    }
  }

  def or(e: BVExpr): BVExpr = {
    if (e.width == 1) { e } else {
      BVNot(BVEqual(e, BVLiteral(0, e.width)))
    }
  }

  def xor(e: BVExpr): BVExpr = {
    if (e.width == 1) { e } else {
      val bits = (0 until e.width).map(ii => BVSlice(e, ii, ii))
      bits.reduce[BVExpr]((a, b) => BVOp(Op.Xor, a, b))
    }
  }
}
