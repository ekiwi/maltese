package maltese

import maltese.smt._
import SMTExprEval._
import scala.math.log10

import scala.io.StdIn.readLine

// General Questions:
// Unfamiliar with all SignalLabel types

class ExecutionEngine(val simplifiedSystem: TransitionSystem) {
  private var memoryMap = collection.mutable.Map[String, BigInt]()
  memoryMap ++= simplifiedSystem.inputs.map(input => input.name -> BigInt(0))
  memoryMap ++= {
    val inits = simplifiedSystem.signals.filter(_.lbl.equals(IsInit)).map(init => init.name -> init).toMap
    simplifiedSystem.states.map(state => state.sym.name -> initState(state, inits))
  }
  private val constraints = simplifiedSystem.signals.filter(_.lbl.equals(IsConstraint))
  private val badStates = simplifiedSystem.signals.filter(_.lbl.equals(IsBad))
  private val transitions = simplifiedSystem.signals.filter(_.lbl.equals(IsNext))

  def execute(): Boolean = {
    //prompt user for input
    for (input <- simplifiedSystem.inputs) {
      memoryMap(input.name) = inputPrompt(input)
    }

    // check constraints
    for (cons <- constraints) { // should constraints be checked before execution?
      val check = eval(cons.e)
      if (check == 0) {
        return false
      }
    }

    // execute transitions
    for (trans <- transitions) {
      memoryMap(trans.name) = eval(trans.e)
    }

    //update states
    for (state <- simplifiedSystem.states) {
      state.next match {
        case s: Some[BVSymbol] => memoryMap(state.sym.name) = memoryMap(s.value.name)
        case _ => printf("No Next Param for %s\n", state.sym.name)
      }
    }

    // check bad states
    for (bad <- badStates) {
      val check = eval(bad.e)
      if (check != 0) {
        return false
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
      case _ => 10/0 //TODO: Add proper error
    }
  }

  private def initState(state: State, inits: Map[String, Signal]): BigInt = { // will init state always be BigInt?
    state.init match {
      case i: Some[BVSymbol] => if (inits.contains(i.value.name)) eval(inits(i.value.name).e) else inputPrompt(state.asInstanceOf[BVSymbol]) // this is probably a bad assumption?
      case _ => inputPrompt(state.asInstanceOf[BVSymbol])
    }

  }

  private def inputPrompt(input: BVSymbol): BigInt = { // how to best implement width-checking?
    var flag = false
    var toRet: BigInt = 0
    do {
      if (flag) {
        printf("Input value must have width of at most %d\n", input.width)
      }
      printf("Input value for %s: ", input.name)
      toRet = BigInt(readLine.toInt)
    } while({
      flag = (log10(toRet.toDouble)/log10(2) + 1).toInt > input.width
      flag
    })
    toRet
  }
}