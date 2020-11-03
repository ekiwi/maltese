package maltese

import maltese.smt._
import SMTExprEval._

import scala.io.StdIn.readLine

class ExecutionEngine(val simplifiedSystem: TransitionSystem) {
  private var memoryMap = collection.mutable.Map[String, BigInt]()
  memoryMap ++= simplifiedSystem.inputs.map(input => input.name -> BigInt(0))
  memoryMap ++= {
    val inits = simplifiedSystem.signals.filter(_.lbl.equals(IsInit)).map(init => init.name -> init).toMap
    simplifiedSystem.states.map(state => state.sym.name -> initState(state.sym.name, inits))
  }
  private val constraints = simplifiedSystem.signals.filter(_.lbl.equals(IsConstraint))
  private val badStates = simplifiedSystem.signals.filter(_.lbl.equals(IsBad))
  private val transitions = simplifiedSystem.signals.filter(_.lbl.equals(IsNext))

  def execute() = {
    //prompt user for input

    // check constraints

    // execute transitions

    // check bad states
  }

  def eval(expr: SMTExpr): BigInt = {
    expr match {
      case e: BVLiteral => e.value
      case e: BVSymbol => memoryMap(e.name)
      case e: BVOp => doBVOp(e.op, eval(e.a), eval(e.b), e.width)
      case e: BVExtend => doBVExtend(eval(e.e), e.width, e.by, e.signed)
      case e: BVEqual => doBVEqual(eval(e.a), eval(e.b))
      case e: BVNot => doBVNot(eval(e.e), e.width)
      case e: BVComparison => doBVCompare(e.op, eval(e.a), eval(e.b), e.width, e.signed)
      case _ => 10/0 //TODO: Add proper error
    }
  }

  def initState(state: String, inits: Map[String, Signal]): BigInt = { // will init state always be BigInt?
    if (inits.contains(state+".init")) {
      val expr = inits(state+".init").e
      return eval(expr)
    }
    inputPrompt()
  }

  def inputPrompt(): BigInt = { // how to best implement width-checking?
    print("Input a value: ")
    BigInt(readLine.toInt)
  }
}