// Copyright 2021 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package chiseltest.symbolic

import firrtl.annotations._
import firrtl.stage.Forms
import firrtl._

import scala.collection.mutable

/** Loads memory initialization files at compile time instead of at simulation/synthesis time.
 *  This works by turning
 * */
object MemoryFileInitPass extends Transform with DependencyAPIMigration {
  // we only want to annotate ground type state
  override def prerequisites = Forms.LowForm
  override def invalidates(a: Transform): Boolean = false

  override protected def execute(state: CircuitState): CircuitState = {
    val loadMem = state.annotations.collect{
      case a: LoadMemoryAnnotation => InitFileAnno(a.target.toTarget, a.fileName, toBase(a.hexOrBinary))
      case a: MemoryFileInlineAnnotation => InitFileAnno(a.target, a.filename, toBase(a.hexOrBinary))
    }

    if(loadMem.isEmpty) return state

    val oldAnnos = state.annotations.filter {
      case _: LoadMemoryAnnotation => false
      case _: MemoryFileInlineAnnotation => false
      case _ => true
    }

    val annos = loadMem.map(convertAnno) ++: oldAnnos
    state.copy(annotations = annos)
  }

  private def convertAnno(a: InitFileAnno): Annotation = {
    val values = load(a.file, a.base)
    if(allTheSame(values)) {
      MemoryScalarInitAnnotation(a.target, values.head)
    } else {
      MemoryArrayInitAnnotation(a.target, values)
    }
  }

  private def allTheSame(value: Seq[BigInt]): Boolean = {
    require(value.nonEmpty)
    value match {
      case Seq(_) => true
      case multiple =>
        val head = multiple.head
        multiple.forall(_ == head)
    }
  }

  private def load(file: String, base: Int): Seq[BigInt] = {
    val path = os.Path(file)
    require(os.exists(path), s"Failed to find memory file: ${path}")
    val content = os.read(path)
    val words = mutable.ArrayBuffer[BigInt]()
    var token = ""
    content.map(_.toLower).foreach {
      case ' ' | '\t' | '\n' =>
        if(token.nonEmpty) {
          val w = BigInt(token, base)
          words.append(w)
          token = ""
        }
      case other =>
        token = token + other
    }
    words.toSeq
  }

  private def toBase(hexOrBinary:  MemoryLoadFileType): Int = hexOrBinary match {
    case MemoryLoadFileType.Hex => 16
    case MemoryLoadFileType.Binary => 2
  }
}

private case class InitFileAnno(target: ReferenceTarget, file: String, base: Int) {
  require(base == 2 || base == 16)
}
