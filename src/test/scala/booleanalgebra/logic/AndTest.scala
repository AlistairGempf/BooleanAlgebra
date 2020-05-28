package booleanalgebra.logic

import org.scalatest.flatspec.AnyFlatSpec

class AndTest extends AnyFlatSpec {
  val w = new LiteralCondition
  val x = new LiteralCondition
  val y = new LiteralCondition
  val z = new LiteralCondition
  "AND.literalise" should "give AND(x, y) for AND(x, y)" in {
    assertResult(AND(Set(x, y)))(AND(Set(x, y)).literalise)
  }
  it should "give AND(x, NOT(y)) for AND(x, NOT(y))" in {
    assertResult(AND(Set(x, NOT(y))))(AND(Set(x, NOT(y))).literalise)
  }
  it should "give AND(x, OR(NOT(y), NOT(z))) for AND(x, NOT(AND(y, z))" in {
    assertResult (AND(Set(x, OR(Set(NOT(y), NOT(z)))))) (AND(Set(x, NOT(AND(Set(y, z))))).literalise)
  }
}
