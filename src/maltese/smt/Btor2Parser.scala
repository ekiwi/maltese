// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.smt

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Btor2 {
  def load(filename: String): TransitionSystem = {
    val ff = Source.fromFile(filename)
    val sys = Btor2Parser.read(ff.getLines())
    ff.close()
    sys
  }
}

object Btor2Parser {
  val unary = Set("not", "inc", "dec", "neg", "redand", "redor", "redxor")
  val binary = Set("iff", "implies", "sgt", "ugt", "sgte", "ugte", "slt", "ult", "slte", "ulte",
    "and", "nand", "nor", "or", "xnor", "xor", "rol", "ror", "sll", "sra", "srl", "add", "mul", "sdiv", "udiv", "smod",
    "srem", "urem", "sub", "saddo", "uaddo", "sdivo", "udivo", "smulo", "umulo", "ssubo", "usubo", "concat")

  def read(lines: Iterator[String]): TransitionSystem = {
    val bvSorts = new mutable.HashMap[Int,Int]()
    val arraySorts = new mutable.HashMap[Int,(Int,Int)]()
    val states = new mutable.HashMap[Int,State]()
    val inputs = new mutable.ArrayBuffer[BVSymbol]()
    val signals = new mutable.HashMap[Int,SMTExpr]()
    val labels = Seq("fair", "bad", "constraint", "output")
      .map{l => l -> new mutable.ArrayBuffer[Tuple2[String, SMTExpr]]()}.toMap
    val yosysLabels = new mutable.HashMap[Int,String]()


    // unique name generator
    val unique_names = new mutable.HashSet[String]()
    def is_unique(name: String): Boolean = !unique_names.contains(name)
    def unique_name(prefix: String): String = Iterator.from(0).map(i => s"_${prefix}_$i").filter(is_unique(_)).next

    // while not part of the btor2 spec, yosys annotates the system's name
    var name: Option[String] = None

    def parseSort(id: Int, parts: Seq[String]): Unit = {
      lazy val i3 = Integer.parseInt(parts(3))
      lazy val i4 = Integer.parseInt(parts(4))
      if(parts(2) == "bitvec") {
        bvSorts(id) = i3
      } else {
        assert(parts(2) == "array")
        arraySorts(id) = (bvSorts(i3), bvSorts(i4))
      }
    }

    /** yosys sometimes provides comments with human readable names for i/o/ and state signals **/
    def parseYosysComment(comment: String): Option[Tuple2[Int,String]] = {
      // yosys module name annotation
      if(comment.contains("Yosys") && comment.contains("for module ")) {
        val start = comment.indexOf("for module ")
        val mod_name = comment.substring(start + "for module ".length).dropRight(1)
        name = Some(mod_name)
      }
      val yosys_lbl: Regex = "\\s*;\\s*(\\d+) \\\\(\\w+)".r
      yosys_lbl.findFirstMatchIn(comment) match {
        case Some(m) => Some((Integer.parseInt(m.group(1)), m.group(2)))
        case None => None
      }
    }

    def parseComment(comment: String): Unit = {
      parseYosysComment(comment) match {
        case Some((ii, lbl)) => yosysLabels(ii) = lbl
        case None => None
      }
    }

    def parseLine(line: String): Unit = {
      if(line.isEmpty) { /* skip blank lines */ return }
      if(line.startsWith(";")) { parseComment(line);  return }
      val parts = line.split(" ")
      val id = Integer.parseInt(parts.head)

      // nodes besides output that feature nid
      def expr(offset: Int): SMTExpr = {
        assert(parts.length > 3 + offset, s"parts(${3 + offset}) does not exist! ${parts.mkString(", ")}")
        val nid = Integer.parseInt(parts(3 + offset))
        assert(signals.contains(nid), s"Unknown node #$nid")
        signals(nid)
      }
      def bvExpr(offset: Int) = expr(offset).asInstanceOf[BVExpr]
      def arrayExpr(offset: Int) = expr(offset).asInstanceOf[ArrayExpr]

      lazy val sortId = Integer.parseInt(parts(2))

      def width: Int = {
        assert(bvSorts.contains(sortId), s"Not a bit vector sort: $line")
        bvSorts(sortId)
      }

      def indexWidth: Int = {
        assert(arraySorts.contains(sortId), s"Not a array sort: $line")
        arraySorts(sortId)._1
      }

      def dataWidth: Int = {
        assert(arraySorts.contains(sortId), s"Not a array sort: $line")
        arraySorts(sortId)._2
      }

      def checkSort(e: SMTExpr): Option[SMTExpr] = e match {
        case b : BVExpr =>
          assert(b.width == width, s"Expected $width-bit value, got ${b.width}-bit value! $line")
          Some(b)
        case a : ArrayExpr =>
          assert(a.indexWidth == indexWidth,
            s"Expected $indexWidth-bit index, got ${a.indexWidth}-bit index! $line")
          assert(a.dataWidth == dataWidth,
            s"Expected $dataWidth-bit data, got ${a.dataWidth}-bit data! $line")
          Some(a)
      }

      def isArray: Boolean = arraySorts.contains(sortId)

      val cmd = parts(1)
      val new_expr = cmd  match {
        case "sort" => parseSort(id, parts) ; None
        case "input" =>
          val name = if(parts.length > 3) parts(3) else unique_name("input")
          assert(is_unique(name))
          unique_names += name
          val input = BVSymbol(name, width)
          inputs.append(input)
          Some(input)
        case lbl @ ("output" | "bad" | "constraint" | "fair") =>
          val name = if(parts.length > 3) parts(3) else unique_name(lbl)
          assert(is_unique(name))
          unique_names += name
          labels(lbl) += (name -> expr(-1))
          None
        case "state" =>
          val name = if(parts.length > 3) parts(3) else unique_name("state")
          assert(is_unique(name))
          unique_names += name
          val sym = if(isArray) ArraySymbol(name, indexWidth, dataWidth) else BVSymbol(name, width)
          states.put(id, State(sym, None, None))
          Some(sym)
        case "next" =>
          val state_id = Integer.parseInt(parts(3))
          states.put(state_id, states(state_id).copy(next=Some(expr(1))))
          None
        case "init" =>
          val state_id = Integer.parseInt(parts(3))
          states.put(state_id, states(state_id).copy(init=Some(expr(1))))
          None
        case format @ ("const" | "constd" | "consth" | "zero" | "one") =>
          val value = if(format == "zero"){ BigInt(0)
          } else if (format == "one") { BigInt(1)
          } else { parseConst(format, parts(3)) }
          checkSort(BVLiteral(value, width))
        case "ones" =>
          checkSort(BVLiteral((BigInt(1) << width) - 1, width))
        case ext @ ("uext" | "sext") =>
          val by = Integer.parseInt(parts(4))
          checkSort(Extend(bvExpr(0), by, signed = ext.startsWith("s")))
        case "slice" =>
          val msb = Integer.parseInt(parts(4))
          val lsb = Integer.parseInt(parts(5))
          checkSort(Slice(bvExpr(0), msb, lsb))
        case op if unary.contains(op) =>
          checkSort(parseUnary(op, bvExpr(0)))
        case "eq" =>
          checkSort(SMTEqual(expr(0), expr(1)))
        case "neq" =>
          checkSort(Not(SMTEqual(expr(0), expr(1))))
        case "concat" =>
          checkSort(BVConcat(bvExpr(0), bvExpr(1)))
        case op if binary.contains(op) =>
          checkSort(parseBinary(op, bvExpr(0), bvExpr(1)))
        case "read" =>
          checkSort(ArrayRead(arrayExpr(0), bvExpr(1)))
        case "write" =>
          checkSort(ArrayStore(arrayExpr(0), bvExpr(1), bvExpr(2)))
        case "ite" =>
          checkSort(SMTIte(bvExpr(0), expr(1), expr(2)))
        case other =>
          throw new RuntimeException(s"Unknown command: $other")

      }
      new_expr match {
        case Some(expr) => signals.put(id, expr)
        case _ =>
      }
    }

    lines.foreach{ ll => parseLine(ll.trim) }

    //println(yosys_lables)
    // TODO: use yosys_lables to fill in missing symbol names

    TransitionSystem(name.getOrElse("Top"),
      inputs=inputs.toSeq,
      states=states.values.toSeq,

      outputs = labels("output").toSeq.map{ case (n,e) => Signal(n,e.asInstanceOf[BVExpr]) },
      constraints = labels("constraint").map(_._2).toSeq.map(_.asInstanceOf[BVExpr]),
      bad = labels("bad").map(_._2).toSeq.map(_.asInstanceOf[BVExpr]),
      fair = labels("fair").map(_._2).toSeq.map(_.asInstanceOf[BVExpr]))
  }

  private def parseConst(format: String, str: String): BigInt = format match {
    case "const" => BigInt(str, 2)
    case "constd" => BigInt(str)
    case "consth" => BigInt(str, 16)
  }

  private def parseUnary(op: String, expr: BVExpr): BVExpr = op match {
    case "not" => Not(expr)
    case "inc" => BVOp(Op.Add, expr, BVLiteral(1, expr.width))
    case "dec" => BVOp(Op.Sub, expr, BVLiteral(1, expr.width))
    case "neg" => Negate(expr)
    case "redand" => ReduceAnd(expr)
    case "redor" => ReduceOr(expr)
    case "redxor" => ReduceXor(expr)
    case other => throw new RuntimeException(s"Unknown unary op $other")
  }

  private def parseBinary(op: String, a: BVExpr, b: BVExpr): BVExpr = op match {
    case "ugt" => BVComparison(Compare.Greater, a, b, signed = false)
    case "ugte" => BVComparison(Compare.GreaterEqual, a, b, signed = false)
    case "ult" => Not(BVComparison(Compare.GreaterEqual, a, b, signed = false))
    case "ulte" => Not(BVComparison(Compare.Greater, a, b, signed = false))
    case "sgt" => BVComparison(Compare.Greater, a, b, signed = true)
    case "sgte" => BVComparison(Compare.GreaterEqual, a, b, signed = true)
    case "slt" => Not(BVComparison(Compare.GreaterEqual, a, b, signed = true))
    case "slte" => Not(BVComparison(Compare.Greater, a, b, signed = true))
    case "and" => BVOp(Op.And, a, b)
    case "nand" => Not(BVOp(Op.And, a, b))
    case "nor" => Not(BVOp(Op.Or, a, b))
    case "or" => BVOp(Op.Or, a, b)
    case "xnor" => Not(BVOp(Op.Xor, a, b))
    case "xor" => BVOp(Op.Xor, a, b)
    case "rol" | "ror" => throw new NotImplementedError("TODO: implement rotates on bv<N>")
    case "sll" => BVOp(Op.ShiftLeft, a, b)
    case "sra" => BVOp(Op.ArithmeticShiftRight, a, b)
    case "srl" => BVOp(Op.ShiftRight, a, b)
    case "add" => BVOp(Op.Add, a, b)
    case "mul" => BVOp(Op.Mul, a, b)
    case "sdiv" => BVOp(Op.SignedDiv, a, b)
    case "udiv" => BVOp(Op.UnsignedDiv, a, b)
    case "smod" => BVOp(Op.SignedMod, a, b)
    case "srem" => BVOp(Op.SignedRem, a, b)
    case "urem" => BVOp(Op.UnsignedRem, a, b)
    case "sub" => BVOp(Op.Sub, a, b)
    case other => throw new RuntimeException(s"Unknown binary op $other")
  }

}