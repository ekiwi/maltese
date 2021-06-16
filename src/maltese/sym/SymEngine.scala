// Copyright 2020-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package maltese.sym

import maltese.mc.TransitionSystem
import maltese.smt._

import scala.collection.mutable

object SymEngine {
  def apply(sys: TransitionSystem, noInit: Boolean = false, opts: Options = Options.Default): SymEngine =
    new SymEngine(sys, noInit, opts)
}

class SymEngine private(sys: TransitionSystem, noInit: Boolean, opts: Options) {
  private val inputs = sys.inputs.map(i => i.name -> i).toMap
  private val states = sys.states.map(s => s.sym.name -> s).toMap
  private val signals = sys.signals.map(s => s.name -> s).toMap
  private val validCellName = (sys.inputs.map(_.name) ++ sys.states.map(_.name) ++ sys.signals.map(_.name)).toSet
  private val getWidth = (sys.inputs.map(i => i.name -> i.width) ++
    sys.states.map(_.sym).collect{ case BVSymbol(name, width) => name -> width } ++
    sys.signals.collect{ case maltese.mc.Signal(name, e: BVExpr, _) => name -> e.width }).toMap
  private val results = mutable.ArrayBuffer[mutable.HashMap[String, BVValueSummary]]()
  /** edges from result to arguments */
  private val uses = mutable.HashMap[Cell, List[Cell]]()
  private implicit val ctx = new SymbolicContext(opts)

  def signalAt(name: String, step: Int): BVValueSummary = signalAt(Cell(name, step))

  private def signalAt(cell: Cell): BVValueSummary = {
    val frame = getFrame(cell.step)
    val r = frame.getOrElseUpdate(cell.signal, computeCell(cell))
    if(r.size > 1000) {
      println(s"WARN: ${cell.id}.size = ${r.size} > 1k")
    }
    // println(s"$name@$step: $r")
    r
  }

  /** removes the result from this cell as well as any cells that depend on it */
  private def invalidate(cell: Cell): Unit = {
    // if the step has not been computed yet, there is nothing to invalidate
    if(results.size < cell.step + 1) return

    // remove the result from the frame
    val frame = results(cell.step)
    frame.remove(cell.signal)

    // remove any cells that depend on this cell
    uses.get(cell) match {
      case None =>
      case Some(Nil) =>
        uses.remove(cell)
      case Some(u) =>
        u.foreach(invalidate)
        uses.remove(cell)
    }
  }

  /** allocates the frame if necessary */
  private def getFrame(step: Int): mutable.HashMap[String, BVValueSummary] = {
    if(results.size < step + 1) {
      (0 to (step - results.size)).foreach(_ => results.append(mutable.HashMap()))
    }
    results(step)
  }

  def set(name: String, step: Int, value: BVValueSummary): Unit = {
    assert(validCellName(name), f"Unknown cell $name")
    val cell = Cell(name, step)
    invalidate(cell)
    val frame = getFrame(cell.step)
    frame(name) = value
  }

  def set(name: String, step: Int, value: BigInt): BVValueSummary = {
    assert(validCellName(name), f"Unknown cell $name")
    val vs = BVValueSummary(BVLiteral(value, getWidth(name)))
    set(name, step, vs)
    vs
  }

  private def computeCell(cell: Cell): BVValueSummary = {
    val name = cell.signal
    if(signals.contains(name)) {
      eval(signals(name).e.asInstanceOf[BVExpr], cell)
    } else if(inputs.contains(name)) {
      inputAt(cell)
    } else if(states.contains(name)) {
      stateAt(cell)
    } else {
      throw new RuntimeException(s"Unknown signal ${cell.id}")
    }
  }
  private def eval(expr: BVExpr, cell: Cell): BVValueSummary = expr match {
    case BVSymbol(name, _) =>
      val prevCell = cell.copy(signal = name)
      // track cell dependencies
      uses(prevCell) = cell +: uses.getOrElse(prevCell, List())
      signalAt(prevCell)
    case l : BVLiteral => BVValueSummary(l)
    case u : BVUnaryExpr => BVValueSummary.unary(eval(u.e, cell), u.reapply)
    case u : BVBinaryExpr => BVValueSummary.binary(eval(u.a, cell), eval(u.b, cell), u.reapply)
    case BVIte(cond, tru, fals) => BVValueSummary.ite(eval(cond, cell), eval(tru, cell), eval(fals, cell))
    case other => throw new RuntimeException(s"Unexpected expression: $other")
  }


  private def stateAt(cell: Cell): BVValueSummary = {
    val state = states(cell.signal)
    if(cell.step == 0) {
      if(state.init.isDefined && !noInit) {
        signalAt(state.name + ".init", 0)
      } else {
        val sym = symbols.getOrElseUpdate(state.name + "@0", SMTSymbol.fromExpr(state.name + "@0", state.sym).asInstanceOf[BVSymbol])
        BVValueSummary(sym)
      }
    } else {
      assert(cell.step > 0)
      signalAt(Cell(state.name + ".next", cell.step - 1))
    }
  }

  private def inputAt(cell: Cell): BVValueSummary = {
    val sym = symbols.getOrElseUpdate(cell.id, BVSymbol(cell.id, inputs(cell.signal).width))
    BVValueSummary(sym)
  }
  private def symbols = mutable.HashMap[String, BVSymbol]()

  def makeSymbol(name: String, width: Int): BVValueSummary = {
    symbols.get(name) match {
      case Some(sym) =>
        assert(sym.width == width)
        BVValueSummary(sym)
      case None =>
        val sym = BVSymbol(name, width)
        symbols(name) = sym
        BVValueSummary(sym)
    }
  }

  def printStatistics(): Unit = {
    ctx.printStatistics()
  }
}

private case class Cell(signal: String, step: Int) {
  def id: String = signal + "@" + step
}