package booleanalgebra.logic

import org.scalatest.flatspec.AnyFlatSpec

class OrTest extends AnyFlatSpec {
  val w = ConditionObject("w")
  val x = ConditionObject("x")
  val y = ConditionObject("y")
  val z = ConditionObject("z")
  "OR.literalise" should "give OR(x, y) for OR(x, y)" in {
    assertResult(OR(Set(x, y)))(OR(Set(x, y)).literalise)
  }
  it should "give OR(x, NOT(y)) for OR(x, NOT(y))" in {
    assertResult(OR(Set(x, NOT(y))))(OR(Set(x, NOT(y))).literalise)
  }
  it should "give OR(x, AND(NOT(y), NOT(z))) for OR(x, NOT(OR(y, z))" in {
    assertResult (OR(Set(x, AND(Set(NOT(y), NOT(z)))))) (OR(Set(x, NOT(OR(Set(y, z))))).literalise)
  }
  "flatten" should "flatten internal ORs" in {
    assertResult(OR(Set(w, x, y, z))) (OR(Set(OR(Set(w, x)), OR(Set(y, z)))).flatten)
  }
  it should "not flatten internal ANDs" in {
    assertResult(OR(Set(AND(Set(w, x)), AND(Set(y, z))))) (OR(Set(AND(Set(w, x)), AND(Set(y, z)))).flatten)
  }
  it should "flatten internal ORs and not flatten internal ANDs" in {
    assertResult(OR(Set(AND(Set(w, x)), y, z))) (OR(Set(AND(Set(w, x)), OR(Set(y, z)))).flatten)
  }
  "annhilate" should "give TrueCondition when there is a true condition" in {
    assertResult(TrueCondition) (OR(Set(TrueCondition, x, y)).annihilate)
  }
  it should "do nothing when there is no true conditions" in {
    assertResult(OR(Set(x, y))) (OR(Set(x, y)).annihilate)
    assertResult(OR(Set(FalseCondition, y))) (OR(Set(FalseCondition, y)).annihilate)
    assertResult(OR(Set(AND(Set(x, z)), y))) (OR(Set(AND(Set(x, z)), y)).annihilate)
  }
  "filterIdentity" should "filter out FalseConditions that are present" in {
    assertResult(OR(Set(y)))(OR(Set(FalseCondition, y)).filterIdentity)
    assertResult(OR(Set(y)))(OR(Set(FalseCondition, FalseCondition, FalseCondition, FalseCondition, FalseCondition, y)).filterIdentity)
    assertResult(OR(Set(AND(Set(x, y)))))(OR(Set(FalseCondition, AND(Set(x, y)))).filterIdentity)
  }
  it should "not do anything when there are no FalseConditions" in {
    assertResult(OR(Set(TrueCondition, x, y))) (OR(Set(TrueCondition, x, y)).filterIdentity)
    assertResult(OR(Set(x, y))) (OR(Set(x, y)).filterIdentity)
    assertResult(OR(Set(AND(Set(x, z)), y))) (OR(Set(AND(Set(x, z)), y)).filterIdentity)
  }
  it should "give FalseCondition when only FalseConditions" in {
    assertResult(FalseCondition)(OR(Set(FalseCondition, FalseCondition, FalseCondition)).filterIdentity)
  }
  "complement" should "give FalseCondition when there is a complement of a literal" in {
    assertResult(FalseCondition)(OR(Set(x, NOT(x))).complement)
    assertResult(FalseCondition)(OR(Set(x, NOT(x), y, z)).complement)
  }
  it should "give FalseCondition when there is a complement of a condition" in {
    assertResult(FalseCondition)(OR(Set(AND(Set(x, y)), NOT(AND(Set(x, y))))).complement)
    assertResult(FalseCondition)(OR(Set(AND(Set(x, y)), NOT(AND(Set(x, y))), z)).complement)
  }
  it should "do nothing when there is not a complement of a literal" in {
    assertResult(OR(Set(x, NOT(w))))(OR(Set(x, NOT(w))).complement)
    assertResult(OR(Set(x, NOT(w), y, z))) (OR(Set(x, NOT(w), y, z)).complement)
  }
  it should "do nothing when there is not a complement of a condition" in {
    assertResult(OR(Set(OR(Set(x, y)), NOT(AND(Set(w, y))))))(OR(Set(OR(Set(x, y)), NOT(AND(Set(w, y))))).complement)
    assertResult(OR(Set(OR(Set(x, y)), NOT(AND(Set(w, y))), z)))(OR(Set(OR(Set(x, y)), NOT(AND(Set(w, y))), z)).complement)
  }
  "eliminate" should "eliminate if only one difference between conditions" in {
    assertResult(OR(Set(x, z))) (OR(Set(AND(Set(x, y)), AND(Set(x, NOT(y))), z)).eliminate)
  }
  it should "eliminate to a single condition" in {
    assertResult(x) (OR(Set(AND(Set(x, y)), AND(Set(x, NOT(y))))).eliminate)
  }
  it should "do nothing if there are no eliminations" in {
    assertResult(OR(Set(AND(Set(x, y)), AND(Set(x, NOT(z)))))) (OR(Set(AND(Set(x, y)), AND(Set(x, NOT(z))))).eliminate)
  }
  it should "do nothing if there are no eliminations because it is not in the dual" in {
    assertResult(OR(Set(OR(Set(x, y)), OR(Set(x, NOT(y)))))) (OR(Set(OR(Set(x, y)), OR(Set(x, NOT(y))))).eliminate)
  }
  "absorb" should "correctly positively absorb" in {
    assertResult(x)(OR(Set(x, AND(Set(x, y)))).absorb)
    assertResult(y)(OR(Set(AND(Set(x, y)), y)).absorb)
    assertResult(OR(Set(x, y)))(OR(Set(AND(Set(OR(Set(x, y)), z)), OR(Set(x, y)))).absorb)
  }
  it should "correctly negatively absorb" in {
    assertResult(OR(Set(x, y)))(OR(Set(x, AND(Set(!x, y)))).absorb)
    assertResult(OR(Set(x, y)))(OR(Set(AND(Set(x, !y)), y)).absorb)
    assertResult(OR(Set(OR(Set(x, y)), z)))(OR(Set(AND(Set(NOT(OR(Set(x, y))), z)), OR(Set(x, y)))).absorb)
  }
  it should "do nothing when there is nothing to absorb" in {
    assertResult(OR(Set(AND(Set(x, y)), AND(Set(x, NOT(y)))))) (OR(Set(AND(Set(x, y)), AND(Set(x, NOT(y))))).absorb)
    assertResult(OR(Set(OR(Set(x, y)), OR(Set(x, NOT(y)))))) (OR(Set(OR(Set(x, y)), OR(Set(x, NOT(y))))).absorb)
  }
}
