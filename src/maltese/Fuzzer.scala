package maltese

import java.io.File

import maltese.mc._
import maltese.passes.{DeadCodeElimination, Inline, Pass, PassManager, PrintSystem, Simplify}

object FuzzerApp extends App {
  var system: TransitionSystem = _
  if(args.length < 1) {
    // println(s"please provide the name of a btor file")
//    val d = "benchmarks/hwmcc19/bv/goel/crafted/toy_lock_4.btor2"
    val d = "benchmarks/custom/simple.btor2"
//    val d = "benchmarks/hwmcc19/bv/goel/crafted/cal10.btor2"
    //val d = "benchmarks/hwmcc19/bv/wolf/2019A/picorv32_mutBX_nomem-p0.btor"
    system = Fuzzer.load(d)
  } else {
    system = Fuzzer.load(args.head)
  }
  Fuzzer.execute(system, 5, 2020, true, false)
}

object Fuzzer {

  private val passes: Iterable[Pass] = Seq(
   Simplify,
    new Inline,
   DeadCodeElimination,

   Simplify,
    new Inline,
   DeadCodeElimination,

    Simplify,

    PrintSystem,
  )

  def load(filename: String): TransitionSystem = {
    // load transition system from file
    val sys = mc.Btor2.load(new File(filename))

    println(s"Loaded $filename")

    val simplified = simplifySystem(sys)

    //val e = getConstraints(simplified, doInit = false)
    //val e = check(sys)

    println()

    return simplified
  }

  def simplifySystem(sys: TransitionSystem): TransitionSystem = PassManager(passes).run(sys, trace = true)

  def execute(system: TransitionSystem, limit:Int = 10, seed:Int = 0, witness: Boolean = false, verbose: Boolean = false) = {
    val exe = new ExecutionEngine(system, witness, verbose)
    val inputsGen = new InputGenerator(seed, verbose)
    var cnt = 0
    var success: Boolean = true
    do {
      cnt+=1
      val inputs = inputsGen.inputsGenerator(system, true)
      success = exe.execute(inputs)
    } while(success & cnt <= limit)
  }
}