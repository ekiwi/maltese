package maltese
import maltese.mc.TransitionSystem
import maltese.smt.BVSymbol

import scala.io.StdIn.readLine
import scala.util.Random

class InputGenerator(val seed: Int, val verbose: Boolean) {
  private val rng = new Random(seed)

  def inputsGenerator(simplifiedSystem: TransitionSystem, random: Boolean): Map[String, BigInt] =
    simplifiedSystem.inputs.map(input => input.name -> { if (random) inputRNG(input) else inputPrompt(input)}).toMap

  def inputPrompt(input: BVSymbol): BigInt = {
    if (verbose) print(f"${input.width}-bit Input value for ${input.name}: ")
    BigInt(readLine.toInt) & ((1 << input.width) - 1)
  }

  def inputRNG(input: BVSymbol): BigInt = { //currently fails constraints pretty easily
    val rand = rng.nextInt(1 << input.width)
    if (verbose) println(f"Input for ${input.name} set to $rand")
    rand
  }
}
