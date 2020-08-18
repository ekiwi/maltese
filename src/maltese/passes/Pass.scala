package maltese.passes

import maltese.smt.TransitionSystem

trait Pass {
  def run(sys: TransitionSystem): TransitionSystem
  def name: String
}
