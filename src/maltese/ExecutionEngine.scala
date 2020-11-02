package maltese

import maltese.smt.{BVLiteral, IsInit, Signal, TransitionSystem}

import scala.io.StdIn.readLine

class ExecutionEngine(val simplifiedSystem: TransitionSystem) {
  private var memoryMap = collection.mutable.Map[String, BigInt]()
  memoryMap ++= simplifiedSystem.inputs.map(input => input.name -> BigInt(0))
  memoryMap ++= {
    val inits = simplifiedSystem.signals.filter(_.lbl.equals(IsInit)).map(init => init.name -> init).toMap
    simplifiedSystem.states.map(state => state.sym.name -> initState(state.sym.name, inits))
  }
  println(memoryMap)

  def initState(state: String, inits: Map[String, Signal]): BigInt = {
    if (inits.contains(state+".initi")) {
      val expr = inits(state+".init").e
      val ret = expr match {
        case e: BVLiteral => e.value
        case _ => printf("Can't assign state to %s\n", expr.getClass.getName)
          inputPrompt()
      }
      return ret
    }
    inputPrompt()

  }

  def inputPrompt(): BigInt = {
    print("Input a value: ")
    BigInt(readLine.toInt)
  }
}