package maltese.youverify

import org.scalatest.flatspec.AnyFlatSpec

class ParserTest extends AnyFlatSpec {
  behavior of "The youverify Parser"

  it should "parse an assignment" in {
    val src =
      """a: INT
        |a = 10
        |""".stripMargin
  }

}
