// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.AnnotationSeq
import firrtl.options.Dependency
import firrtl.stage.{FirrtlSourceAnnotation, FirrtlStage, RunFirrtlTransformAnnotation}
import maltese.mc
import maltese.mc._
import maltese.passes.{CreateInitAndNextSignals, DeadCodeElimination, Inline, Pass, PassManager, Simplify}
import maltese.sym._

import java.io.File
import scala.collection.mutable


class SymbolicSim(sys: TransitionSystem, ignoreAsserts: Boolean) {
  // we do not support assumptions at the moment
  private val assumptions =  sys.signals.filter(_.lbl == IsConstraint)
  require(assumptions.isEmpty, "Assumptions are currently not supported!")

  // we need to assertion names if we want to check them
  private val assertions = sys.signals.filter(_.lbl == IsBad).map(_.name)

  // we need to cache the input assignment to retain the in the next cycle
  private val isInput = sys.inputs.map(_.name).toSet
  private val inputAssignments = mutable.HashMap[String, BVValueSummary]()

  private val engine = SymEngine(sys, noInit = false)
  private var cycleCount = 0
  doCheckAsserts() // check assertions in initial state

  def step(): Unit = {
    cycleCount += 1
    // apply any "sticky" pokes
    inputAssignments.foreach { case (signal, value) => engine.set(signal, cycleCount, value) }
    doCheckAsserts()
  }

  def peek(signal: String): Value = {
    new Value(engine.signalAt(signal, cycleCount))
  }

  def pokeDontCare(signal: String): Unit = {
    engine.invalidate(signal, cycleCount)
    inputAssignments.remove(signal)
  }

  def poke(signal: String, value: BigInt): Unit = {
    val vs = engine.set(signal, cycleCount, value)
    if(isInput(signal)) { inputAssignments(signal) = vs }
  }

  def poke(signal: String, value: Value): Unit = {
    val vs = Value.getValueSummary(value)
    engine.set(signal, cycleCount, vs)
    if(isInput(signal)) { inputAssignments(signal) = vs }
  }

  def makeSymbol(name: String, width: Int): Value = {
    new Value(engine.makeSymbol(name, width))
  }

  def assert(v: Value, msg: => String = ""): Unit = {
    val vs = Value.getValueSummary(v)
    if(vs.isSat) {
      if(vs.isConcrete) {
        throw new RuntimeException(s"Assertion always fails! $msg")
      } else {
        throw new RuntimeException(s"Assertion may fail! $msg")
      }
    }
  }

  private def doCheckAsserts(): Unit = if(!ignoreAsserts) {
    assertions.foreach{ a =>
      assert(peek(a), s"$a in cycle $cycleCount")
    }
  }
}


class Value(private val e: BVValueSummary) {
  def isSymbolic: Boolean = e.isSymbolic
  def isConcrete: Boolean = e.isConcrete
  def getValue: BigInt = e.value match {
    case Some(v) => v
    case None => throw new RuntimeException(
      s"Value is symbolic: ${e.symbolic}"
    )
  }
  override def toString = e.value match {
    case Some(value) => "Value(" + value + ")"
    case None => "Value(" + e.toString + ")"
  }
}

private object Value {
  def getValueSummary(v: Value): BVValueSummary = v.e
}


object SymbolicSim {
  // TODO: add function to start with firrtl or a Chisel circuit

  def loadBtor(filename: String): SymbolicSim = loadBtor(filename, false)
  def loadBtor(filename: String, ignoreAsserts: Boolean): SymbolicSim = {
    // load transition system from file
    val sys = mc.Btor2.load(new File(filename))
    val simplified = simplifySystem(sys)
    new SymbolicSim(simplified, ignoreAsserts = ignoreAsserts)
  }

  def loadBtor(src: String, name: String): SymbolicSim = loadBtor(src, name, false)
  def loadBtor(src: String, name: String, ignoreAsserts: Boolean): SymbolicSim = {
    val sys = mc.Btor2.read(src = src, defaultName = name)
    val simplified = simplifySystem(sys)
    if(verbose) { println(simplified.serialize) }
    new SymbolicSim(simplified, ignoreAsserts = ignoreAsserts)
  }

  private lazy val firrtlCompiler = new FirrtlStage
  private def firrtlCompilerSource(src: String) = Seq(FirrtlSourceAnnotation(src))

  def loadFirrtl(src: String): SymbolicSim = loadFirrtl(src, List(), false)
  def loadFirrtl(src: String, annos: AnnotationSeq): SymbolicSim = loadFirrtl(src, annos, false)
  def loadFirrtl(src: String, annos: AnnotationSeq, ignoreAsserts: Boolean): SymbolicSim = {
    val r = firrtlCompiler.execute(Array("-E", "experimental-btor2"), firrtlCompilerSource(src) ++ annos)
    val sys = firrtl.backends.experimental.smt.ExpressionConverter.toMaltese(r).getOrElse {
      throw new RuntimeException(s"Failed to extract transition system from: $r")
    }
    val simplified = simplifySystem(sys)
    if(verbose) { println(simplified.serialize) }
    new SymbolicSim(simplified, ignoreAsserts = ignoreAsserts)
  }

  private val verbose = false
  private def simplifySystem(sys: TransitionSystem): TransitionSystem = PassManager(passes).run(sys, trace = verbose)
  private val passes: Iterable[Pass] = Seq(
    CreateInitAndNextSignals,
    Simplify,
    new Inline,
    new DeadCodeElimination(removeUnusedInputs = true),

    Simplify,
    new Inline,
    new DeadCodeElimination(removeUnusedInputs = true),

    Simplify,
    // PrintSystem,
  )
}
