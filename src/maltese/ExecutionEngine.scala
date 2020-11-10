package maltese

import maltese.smt._
import SMTExprEval._
import maltese.mc._
import scala.io.StdIn.readLine
import scala.util.Random

// General Questions:

class ExecutionEngine(val simplifiedSystem: TransitionSystem, val witness: Boolean) {
  private var memoryMap = collection.mutable.Map[String, BigInt]()
  memoryMap ++= simplifiedSystem.inputs.map(input => input.name -> BigInt(0))
  memoryMap ++= {
    val inits = simplifiedSystem.signals.filter(_.lbl == IsInit).map(init => init.name -> init).toMap
    simplifiedSystem.states.map(state => state.sym.name -> {
      state.init match {
        case Some(i: BVSymbol) => if (inits.contains(i.name)) eval(inits(i.name).e) else inputPrompt(state.sym.asInstanceOf[BVSymbol])
        case _ => inputPrompt(state.sym.asInstanceOf[BVSymbol])
      }
    })
  }
  private val transitions = simplifiedSystem.signals.filter(_.lbl != IsInit)
  private var inputsList = collection.mutable.ArrayBuffer.empty[Map[String, BigInt]]
  private val rng = Random

  def inputsGenerator(random: Boolean): Map[String, BigInt] =
    simplifiedSystem.inputs.map(input => input.name -> { if (random) inputRNG(input) else inputPrompt(input)}).toMap
  private def inputPrompt(input: BVSymbol): BigInt = {
    print(f"Input value for ${input.name}: ")
    BigInt(readLine.toInt) & ((1 << input.width) - 1)
  }
  private def inputRNG(input: BVSymbol): BigInt = { //currently fails constraints pretty easily
    val rand = rng.nextInt(4) //rng.nextInt(1 << input.width)
    println(f"Input for ${input.name} set to $rand")
    rand
  }

  def execute(inputs: Map[String, BigInt]): Boolean = {
    //initialize input. missing values default to 0
    for (input <- simplifiedSystem.inputs) {
      memoryMap(input.name) = inputs.getOrElse(input.name, 0)
    }
    if (witness) {
      inputsList += inputs
    }

    // execute transitions
    var value: BigInt = 0
    for (trans <- transitions) {
      value = eval(trans.e)
      memoryMap(trans.name) = value
      trans.lbl match {
//        case IsNext => {
//          val state = trans.name.split("\\.")(0)
//          memoryMap(state) = memoryMap(trans.name)
//        }
        case IsConstraint => if (value == 0) return failCase(trans)
        case IsBad => if (value != 0) return failCase(trans)
        case IsOutput => println(s"Output for ${trans.name}: $value")
        case _ =>
      }
    }

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
    }
  }

  private def failCase(transition: Signal): Boolean = {
    println(f"System failed on ${transition.name} of type ${transition.lbl} with value ${eval(transition.e)}")
    var cnt = 0
    for (input <- inputsList) {
      println(f"At time t=$cnt, the following mapping was used: $input")
      cnt+=1
    }
    false
  }

}