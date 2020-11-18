package maltese

import maltese.smt._
import SMTExprEval._
import maltese.mc._
import treadle.vcd

class ExecutionEngine(val simplifiedSystem: TransitionSystem, val witness: Option[String], val verbose: Boolean) {
  private var memoryMap = collection.mutable.Map[String, BigInt]()
  private val inputsGen = new InputGenerator(0, true)
  private var vcdWriter: Option[vcd.VCD] = if (witness.isEmpty) None else {
    val vv = vcd.VCD(simplifiedSystem.name)
    vv.addWire("Step", 64)
    simplifiedSystem.inputs.foreach(s => vv.addWire(s.name, s.width))
    simplifiedSystem.states.foreach(s => vv.addWire(s.name, s.sym.asInstanceOf[BVSymbol].width))
    Some(vv)
  }

  memoryMap ++= {
    val inits = simplifiedSystem.signals.filter(_.lbl == IsInit).map(init => init.name -> init).toMap
    simplifiedSystem.states.map(state => state.name -> {
      state.init match {
        case Some(i: BVSymbol) => if (inits.contains(i.name)) eval(inits(i.name).e) else inputsGen.inputPrompt(state.sym.asInstanceOf[BVSymbol])
        case _ => inputsGen.inputPrompt(state.sym.asInstanceOf[BVSymbol])
      }
    })
  }
  private val transitions = simplifiedSystem.signals.filter(_.lbl != IsInit)
  private var inputsList = collection.mutable.ArrayBuffer.empty[Map[String, BigInt]]


  def execute(inputs: Map[String, BigInt]): Boolean = {
    //initialize input. missing values default to 0
    for (input <- simplifiedSystem.inputs) {
      memoryMap(input.name) = inputs.getOrElse(input.name, 0)
    }

    //print states
    if (verbose) {
      for (state <- simplifiedSystem.states) {
        println(f"${state.name} has value ${memoryMap(state.name)}")
      }
    }

    if (witness.nonEmpty) {
      val vv = vcdWriter.get
      simplifiedSystem.inputs.foreach(s => vv.wireChanged(s.name, memoryMap(s.name)))
      simplifiedSystem.states.foreach(s => vv.wireChanged(s.name, memoryMap(s.name)))

    }

    // execute transitions
    var value: BigInt = 0
    for (trans <- transitions) {
      value = eval(trans.e)
      memoryMap(trans.name) = value
      trans.lbl match {
        case IsConstraint => if (value == 0) return failCase(trans)
        case IsBad => if (value != 0) return failCase(trans)
        case IsOutput => if (verbose) println(s"Output for ${trans.name} is $value")
        case _ =>
      }
    }

    vcdWriter.foreach(vv => vv.incrementTime())

    //update states
    for (state <- simplifiedSystem.states) {
      state.next match {
        case Some(s: SMTExpr) => memoryMap(state.sym.name) = eval(s)
        case _ => println(s"No Next Param for ${state.sym.name}")
      }
    }

    //return true if everything went well
    true
  }

  private def eval(expr: SMTExpr): BigInt = {
    expr match {
      case e: BVLiteral => e.value
      case e: BVSymbol => memoryMap(e.name)
      case e: BVExtend => doBVExtend(eval(e.e), e.width, e.by, e.signed)
      case e: BVSlice => doBVSlice(eval(e.e), e.hi, e.lo)
      case e: BVNot => doBVNot(eval(e.e), e.width)
      case e: BVNegate => doBVNegate(eval(e.e), e.width)
      case e: BVEqual => doBVEqual(eval(e.a), eval(e.b))
      case e: BVComparison => doBVCompare(e.op, eval(e.a), eval(e.b), e.width, e.signed)
      case e: BVOp => doBVOp(e.op, eval(e.a), eval(e.b), e.width)
      case e: BVConcat => doBVConcat(eval(e.a), eval(e.b), e.width)
      case e: BVIte => if (eval(e.cond)!=0) eval(e.tru) else eval(e.fals)
    }
  }

  private def failCase(transition: Signal): Boolean = {
    println(f"System failed on ${transition.name} of type ${transition.lbl} with value ${eval(transition.e)}")
    if (witness.nonEmpty) {
      val vv = vcdWriter.get
      vv.incrementTime()
      vv.write(witness.get)
    }
    false
  }

}