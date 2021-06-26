// Copyright 2020-2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.analyses.InstanceKeyGraph
import firrtl.analyses.InstanceKeyGraph.InstanceKey
import firrtl.annotations._
import firrtl.options.Dependency
import firrtl.passes.InlineAnnotation
import firrtl.stage.Forms
import firrtl.transforms.{DontTouchAllTargets}
import firrtl._

/** tracks how a reference is renamed */
case class ReferenceAnno(target: ReferenceTarget, oldName: String) extends SingleTargetAnnotation[ReferenceTarget] with DontTouchAllTargets {
  override def duplicate(n: ReferenceTarget) = copy(target = n)
}

/** flattens the complete circuit */
object FlattenPass extends Transform with DependencyAPIMigration {
  // we only want to annotate ground type state
  override def prerequisites = Forms.LowForm
  // this pass relies on modules not being dedupped
  override def optionalPrerequisiteOf = Seq(Dependency[firrtl.passes.InlineInstances])
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val iGraph = InstanceKeyGraph(state.circuit)
    val children = iGraph.getChildInstances.toMap

    // we tag every module to be inlined unless it is explicitly marked as doNotInline
    val cRef = CircuitTarget(state.circuit.main)
    val main = cRef.module(state.circuit.main)
    val doNotInline = Set[String]()
    val inlineAnnos = inlines(main, main)(children, iGraph.moduleMap, doNotInline)

    val annos = state.annotations ++ inlineAnnos
    state.copy(annotations = annos)
  }

  private def inlines(m: ModuleTarget, inst: IsModule)(implicit children: Map[String, Seq[InstanceKey]], moduleMap: Map[String, ir.DefModule], doNotInline: Set[String]): AnnotationSeq = {
    if(doNotInline.contains(m.module)) { Seq() } else {
      val childAnnos = children(m.module).flatMap(c => inlines(m.targetParent.module(c.module), m.instOf(c.name, c.module)))
      if(m.circuit == m.module) { // never inline the main module
        childAnnos
      } else {
        val stateAnnos = moduleMap(m.module) match {
          case mod: ir.Module =>
            val prefix = toPath(inst).split('.').drop(1).mkString(".") + "."
            onStmt(mod.body, inst, prefix)
          case _ => List()
        }
        stateAnnos ++: InlineAnnotation(toName(m)) +: childAnnos
      }
    }
  }

  // annotate state elements
  private def onStmt(s: ir.Statement, m: IsModule, prefix: String): Seq[Annotation] = s match {
    case r: ir.DefRegister =>
      List(ReferenceAnno(m.ref(r.name), prefix + r.name))
    case m: ir.DefMemory =>
      // TODO
      List()
    case ir.Block(stmts) => stmts.flatMap(onStmt(_, m, prefix))
    case _ => List()
  }

  /** the InlineInstances pass uses Name instead of Target  */
  private def toName(m: ModuleTarget): ModuleName = ModuleName(m.module, CircuitName(m.circuit))

  def toPath(t: Target): String = {
    val tokenString = t.tokens.flatMap {
      case TargetToken.Ref(r) => Some("." + r)
      case TargetToken.Instance(i) => Some("." + i)
      case TargetToken.Field(f) => Some("." + f)
      case TargetToken.Index(v) => Some("[" + v + "]")
      case _ => None
    }.mkString("")
    t.moduleOpt.getOrElse("") + tokenString
  }

  def getRenames(annos: AnnotationSeq): Map[String, String] = annos
    .collect{ case ReferenceAnno(target, oldName) => oldName -> toPath(target).split('.').drop(1).mkString(".") }
    .filterNot{ case (o, n) => o == n }
    .toMap
}
