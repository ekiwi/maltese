// Copyright 2020 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>


package maltese.passes
import maltese.mc
import scala.collection.mutable

/** Analyze signal dependencies.
 *
 */
class AnalyzeDependencies extends Pass {
  override def name = "AnalyzeDependencies"

  override def run(sys: mc.TransitionSystem): mc.TransitionSystem = {
    val constraints = sys.signals.filter(_.lbl == mc.IsConstraint).map(_.name)
    if(constraints.isEmpty) {
      println("WARN: no constraints")
      return sys
    }

    val signalDeps = sys.signals.map { s => s.name -> Analysis.findSymbols(s.e).map(_.name) }
    val leaves = (sys.inputs.map(_.name) ++ sys.states.map(_.name)).map(_ -> List())
    val dependencyEdges = (signalDeps ++ leaves).toMap

    // TODO: take state init into account

    val isInput = sys.inputs.map(_.name).toSet
    val hasInit = sys.states.filter(_.init.nonEmpty).map(_.name).toSet

    println("Constraint dependencies:")
    constraints.foreach { c =>
      val deps = getDependencies(c, dependencyEdges)
      val (inputs, states) = deps.partition(isInput)
      val (initStates, nonInitStates) = states.partition(hasInit)
      println(s"$c: ${inputs.length} INPUTS + ${initStates.length} STATES w/ INIT + ${nonInitStates.length} STATES w/o INIT ")
      println("  " + (inputs ++ initStates ++ nonInitStates).mkString(", "))
    }

    sys
  }

  private def getDependencies(name: String, deps: Map[String, List[String]]): List[String] = {
    val todo = mutable.Stack[String]()
    val visited = mutable.HashSet[String]()
    var res = List[String]()
    todo.push(name)
    while(todo.nonEmpty) {
      val node = todo.pop()
      visited.add(node)
      val children = deps(node)
      if(children.isEmpty) {
        res = node +: res
      } else {
        deps(node).filterNot(visited).foreach(todo.push)
      }
    }
    res
  }
}
