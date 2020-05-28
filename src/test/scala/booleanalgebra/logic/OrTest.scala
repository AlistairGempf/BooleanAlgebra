package booleanalgebra.logic

import org.scalatest.flatspec.AnyFlatSpec

class OrTest extends AnyFlatSpec {
  val w = new LiteralCondition
  val x = new LiteralCondition
  val y = new LiteralCondition
  val z = new LiteralCondition
  "OR.literalise" should "give OR(x, y) for OR(x, y)" in {
    assertResult(OR(Set(x, y)))(OR(Set(x, y)).literalise)
  }
  it should "give OR(x, NOT(y)) for OR(x, NOT(y))" in {
    assertResult(OR(Set(x, NOT(y))))(OR(Set(x, NOT(y))).literalise)
  }
  it should "give OR(x, AND(NOT(y), NOT(z))) for OR(x, NOT(OR(y, z))" in {
    assertResult (OR(Set(x, AND(Set(NOT(y), NOT(z)))))) (OR(Set(x, NOT(OR(Set(y, z))))).literalise)
  }
}
