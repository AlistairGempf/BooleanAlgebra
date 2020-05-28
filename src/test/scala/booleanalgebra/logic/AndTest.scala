package booleanalgebra.logic

import booleanalgebra.ConditionLiteral
import org.scalatest.flatspec.AnyFlatSpec

case class ConditionObject(conditionObject: String) extends ConditionLiteral {
  override def toString = {
    s"${conditionObject}"
  }
}
class AndTest extends AnyFlatSpec {
  val w = ConditionObject("w")
  val x = ConditionObject("x")
  val y = ConditionObject("y")
  val z = ConditionObject("z")
  "AND.literalise" should "give AND(x, y) for AND(x, y)" in {
    assertResult(AND(Set(x, y)))(AND(Set(x, y)).literalise)
  }
  it should "give AND(x, NOT(y)) for AND(x, NOT(y))" in {
    assertResult(AND(Set(x, NOT(y))))(AND(Set(x, NOT(y))).literalise)
  }
  it should "give AND(x, OR(NOT(y), NOT(z))) for AND(x, NOT(AND(y, z))" in {
    assertResult (AND(Set(x, OR(Set(NOT(y), NOT(z)))))) (AND(Set(x, NOT(AND(Set(y, z))))).literalise)
  }
  "flatten" should "flatten internal ANDs" in {
    assertResult(AND(Set(w, x, y, z))) (AND(Set(AND(Set(w, x)), AND(Set(y, z)))).flatten)
  }
  it should "not flatten internal ORs" in {
    assertResult(AND(Set(OR(Set(w, x)), OR(Set(y, z))))) (AND(Set(OR(Set(w, x)), OR(Set(y, z)))).flatten)
  }
  it should "flatten internal ANDs and not flatten internal ORs" in {
    assertResult(AND(Set(OR(Set(w, x)), y, z))) (AND(Set(OR(Set(w, x)), AND(Set(y, z)))).flatten)
  }
  "annhilate" should "give FalseCondition when there is a false conditions" in {
    assertResult(FalseCondition) (AND(Set(FalseCondition, x, y)).annihilate)
  }
  it should "do nothing when there is no false conditions" in {
    assertResult(AND(Set(x, y))) (AND(Set(x, y)).annihilate)
    assertResult(AND(Set(TrueCondition, y))) (AND(Set(TrueCondition, y)).annihilate)
    assertResult(AND(Set(OR(Set(x, z)), y))) (AND(Set(OR(Set(x, z)), y)).annihilate)
  }
  "filterIdentity" should "filter out TrueConditions" in {
    assertResult(AND(Set(y)))(AND(Set(TrueCondition, y)).filterIdentity)
    assertResult(AND(Set(OR(Set(x, y)))))(AND(Set(TrueCondition, OR(Set(x, y)))).filterIdentity)
    assertResult(AND(Set(y)))(AND(Set(TrueCondition, TrueCondition, TrueCondition, TrueCondition, TrueCondition, y)).filterIdentity)
  }
  it should "not do anything when there is not a TrueCondition" in {
    assertResult(AND(Set(FalseCondition, x, y))) (AND(Set(FalseCondition, x, y)).filterIdentity)
    assertResult(AND(Set(x, y))) (AND(Set(x, y)).filterIdentity)
    assertResult(AND(Set(OR(Set(x, z)), y))) (AND(Set(OR(Set(x, z)), y)).filterIdentity)
  }
  it should "give TrueCondition when only TrueCondition" in {
    assertResult(TrueCondition)(AND(Set(TrueCondition, TrueCondition, TrueCondition)).filterIdentity)
  }
  "complement" should "give FalseCondition when there is a complement of a literal" in {
    assertResult(FalseCondition)(AND(Set(x, NOT(x))).complement)
    assertResult(FalseCondition)(AND(Set(x, NOT(x), y, z)).complement)
  }
  it should "give FalseCondition when there is a complement of a condition" in {
    assertResult(FalseCondition)(AND(Set(OR(Set(x, y)), NOT(OR(Set(x, y))))).complement)
    assertResult(FalseCondition)(AND(Set(OR(Set(x, y)), NOT(OR(Set(x, y))), z)).complement)
  }
  it should "do nothing when there is not a complement of a literal" in {
    assertResult(AND(Set(x, NOT(w))))(AND(Set(x, NOT(w))).complement)
    assertResult(AND(Set(x, NOT(w), y, z))) (AND(Set(x, NOT(w), y, z)).complement)
  }
  it should "do nothing when there is not a complement of a condition" in {
    assertResult(AND(Set(OR(Set(x, y)), NOT(OR(Set(w, y))))))(AND(Set(OR(Set(x, y)), NOT(OR(Set(w, y))))).complement)
    assertResult(AND(Set(OR(Set(x, y)), NOT(OR(Set(w, y))), z)))(AND(Set(OR(Set(x, y)), NOT(OR(Set(w, y))), z)).complement)
  }
  "eliminate" should "eliminate if only one difference between conditions" in {
    assertResult(AND(Set(x, z))) (AND(Set(OR(Set(x, y)), OR(Set(x, NOT(y))), z)).eliminate)
  }
  it should "eliminate to a single condition" in {
    assertResult(x) (AND(Set(OR(Set(x, y)), OR(Set(x, NOT(y))))).eliminate)
  }
  it should "do nothing if there are no eliminations" in {
    assertResult(AND(Set(OR(Set(x, y)), OR(Set(x, NOT(z)))))) (AND(Set(OR(Set(x, y)), OR(Set(x, NOT(z))))).eliminate)
  }
  it should "do nothing if there are no eliminations because it is not in the dual" in {
    assertResult(AND(Set(AND(Set(x, y)), AND(Set(x, NOT(y)))))) (AND(Set(AND(Set(x, y)), AND(Set(x, NOT(y))))).eliminate)
  }
  "absorb" should "correctly positively absorb" in {
    assertResult(x)(AND(Set(x, OR(Set(x, y)))).absorb)
    assertResult(y)(AND(Set(OR(Set(x, y)), y)).absorb)
    assertResult(AND(Set(x, y)))(AND(Set(OR(Set(AND(Set(x, y)), z)), AND(Set(x, y)))).absorb)
  }
  it should "correctly negatively absorb" in {
    assertResult(AND(Set(x, y)))(AND(Set(x, OR(Set(!x, y)))).absorb)
    assertResult(AND(Set(x, y)))(AND(Set(OR(Set(x, !y)), y)).absorb)
    assertResult(AND(Set(AND(Set(x, y)), z)))(AND(Set(OR(Set(NOT(AND(Set(x, y))), z)), AND(Set(x, y)))).absorb)
  }
  it should "do nothing when there is nothing to absorb" in {
    assertResult(AND(Set(OR(Set(x, y)), OR(Set(x, NOT(y)))))) (AND(Set(OR(Set(x, y)), OR(Set(x, NOT(y))))).absorb)
    assertResult(AND(Set(AND(Set(x, y)), AND(Set(x, NOT(y)))))) (AND(Set(AND(Set(x, y)), AND(Set(x, NOT(y))))).absorb)
  }
  "simplify" should "work on relatively simple things" in {
    assertResult(x && z)((x && (y || z) && (!y || z)).simplify)
    assertResult(FalseCondition)((x && (y || z) && (!y || z) && !z).simplify)
  }
}
