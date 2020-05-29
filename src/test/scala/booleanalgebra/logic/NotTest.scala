package booleanalgebra.logic

import org.scalatest.flatspec.AnyFlatSpec

class NotTest extends AnyFlatSpec {
  val w = new LiteralCondition
  val x = new LiteralCondition
  val y = new LiteralCondition
  val z = new LiteralCondition
  "literalising NOT(TrueCondition)" should "give FalseCondition" in {
    assertResult(FalseCondition)(NOT(TrueCondition).literalise)
  }
  "literalising NOT(FalseCondition)" should "give TrueCondition" in {
    assertResult(TrueCondition)(NOT(FalseCondition).literalise)
  }
  "literalising NOT(x)" should "give NOT(x)" in {
    assertResult(NOT(x))(NOT(x).literalise)
  }
  "literalising NOT(NOT(x))" should "give x" in {
    assertResult(x)(NOT(NOT(x)).literalise)
  }
  "literalising NOT(OR(x, y))" should "give AND(NOT(x), NOT(y))" in {
    assertResult(AND(Set(NOT(x), NOT(y))))(NOT(OR(Set(x, y))).literalise)
  }
  "literalising NOT(AND(x, y))" should "give OR(NOT(x), NOT(y))" in {
    assertResult(OR(Set(NOT(x), NOT(y))))(NOT(AND(Set(x, y))).literalise)
  }
  "literalising NOT(OR(x, NOT(y)))" should "give AND(NOT(x), y)" in {
    assertResult(AND(Set(NOT(x), y)))(NOT(OR(Set(x, NOT(y)))).literalise)
  }
  "literalising AND(NOT(OR(x, y)), z)" should "give AND(NOT(x), NOT(y), z)" in {
    assertResult(AND(Set(NOT(x), NOT(y), z)))(AND(Set(NOT(OR(Set(x, y))), z)).literalise)
  }
}
