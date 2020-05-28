package booleanalgebra.logic
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class ConditionTest extends AnyFlatSpec {
  val w = new LiteralCondition
  val x = new LiteralCondition
  val y = new LiteralCondition
  val z = new LiteralCondition
  "True && False" should "give False" in {
    assertResult(FalseCondition)(TrueCondition && FalseCondition)
  }
  "False && False" should "give False" in {
    assertResult(FalseCondition)(FalseCondition && FalseCondition)
  }
  "True && True" should "give True" in {
    assertResult(TrueCondition)(TrueCondition && TrueCondition)
  }
  "x && y" should "give AND(x, y)" in {
    assertResult(AND(Set(x, y)))(x && y)
  }
  "True && (AND(x, y))" should "give AND(x, y)" in {
    assertResult(AND(Set(x, y)))(TrueCondition && AND(Set(x, y)))
  }
  "(AND(x, y)) && FalseCondition" should "give False" in {
    assertResult(FalseCondition)(AND(Set(x, y)) && FalseCondition)
  }
  "(AND(x, y)) && z" should "give AND(x, y, z)" in {
    assertResult(AND(Set(x, y, z)))(AND(Set(x, y)) && z)
  }
  "(AND(w, x)) && AND(y, z)" should "give AND(w, x, y, z)" in {
    assertResult(AND(Set(w, x, y, z)))(AND(Set(w, x)) && AND(Set(y, z)))
  }
  "(OR(w, x)) && OR(y, z)" should "give AND((OR(w, x)), OR(y, z))" in {
    assertResult(AND(Set(OR(Set(w, x)), OR(Set(y, z)))))(OR(Set(w, x)) && OR(Set(y, z)))
  }
  "(OR(w, x)) && AND(y, z)" should "give AND((OR(w, x)), y, z)" in {
    assertResult(AND(Set(OR(Set(w, x)), y, z)))(OR(Set(w, x)) && AND(Set(y, z)))
  }


  "True || False" should "give True" in {
    assertResult(TrueCondition)(TrueCondition || FalseCondition)
  }
  "False || False" should "give False" in {
    assertResult(FalseCondition)(FalseCondition || FalseCondition)
  }
  "True || True" should "give True" in {
    assertResult(TrueCondition)(TrueCondition || TrueCondition)
  }
  "x || y" should "give OR(x, y)" in {
    assertResult(OR(Set(x, y)))(x || y)
  }
  "False || (OR(x, y))" should "give OR(x, y)" in {
    assertResult(OR(Set(x, y)))(FalseCondition || OR(Set(x, y)))
  }
  "(OR(x, y)) || TrueCondition" should "give True" in {
    assertResult(TrueCondition)(OR(Set(x, y)) || TrueCondition)
  }
  "(OR(x, y)) || z" should "give OR(x, y, z)" in {
    assertResult(OR(Set(x, y, z)))(OR(Set(x, y)) || z)
  }
  "(OR(w, x)) || OR(y, z)" should "give OR(w, x, y, z)" in {
    assertResult(OR(Set(w, x, y, z)))(OR(Set(w, x)) || OR(Set(y, z)))
  }
  "(AND(w, x)) || AND(y, z)" should "give OR((AND(w, x)), AND(y, z))" in {
    assertResult(OR(Set(AND(Set(w, x)), AND(Set(y, z)))))(AND(Set(w, x)) || AND(Set(y, z)))
  }
  "(OR(w, x)) || AND(y, z)" should "give OR((w, x, AND(y, z))" in {
    assertResult(OR(Set(w, x, AND(Set(y, z)))))(OR(Set(w, x)) || AND(Set(y, z)))
  }

  "Applying a condition to a literal" should "result in true when in true" in {
    assertResult(TrueCondition)(x(Set(x)))
  }
  it should "result in false when not in true and there is no false" in {
    assertResult(FalseCondition)(x(Set(y)))
  }
  it should "result in true when in true and a different false" in {
    assertResult(TrueCondition)(x(Set(x), Set(y)))
  }
  it should "result in original condition when not in true and not in false" in {
    assertResult(x)(x(Set(z), Set(y)))
  }
  it should "result in false when different true and in false" in {
    assertResult(FalseCondition)(x(Set(y, z), Set(x, w)))
  }
  it should "throw if in true and false" in {
    assertThrows[IllegalArgumentException](x(Set(x, y, z), Set(x, w)))
  }

  "Applying a condition to a NOT(literal)" should "result in false when in true" in {
    assertResult(FalseCondition)(NOT(x)(Set(x)))
  }
  it should "result in true when not in true and there is no false" in {
    assertResult(TrueCondition)(NOT(x)(Set(y)))
  }
  it should "result in false when in true and a different false" in {
    assertResult(FalseCondition)(NOT(x)(Set(x), Set(y)))
  }
  it should "result in original condition when not in true and not in false" in {
    assertResult(NOT(x))(NOT(x)(Set(z), Set(y)))
  }
  it should "result in true when different true and in false" in {
    assertResult(TrueCondition)(NOT(x)(Set(y, z), Set(x, w)))
  }
  it should "throw if in true and false" in {
    assertThrows[IllegalArgumentException](NOT(x)(Set(x, y, z), Set(x, w)))
  }

  "Applying a condition to a AND(x, y)" should "result in false when only one in true" in {
    assertResult(FalseCondition)((x && y)(Set(x)))
  }
  it should "result in false when neither in true" in {
    assertResult(FalseCondition)((x && y)(Set()))
  }
  it should "result in true when both in true" in {
    assertResult(TrueCondition)((x && y)(Set(x, y)))
  }
  it should "result in the AND(x, y) when neither in true or false" in {
    assertResult(AND(Set(x, y)))((x && y)(Set(), Set()))
  }
  it should "result in the y when x is true and y is neither in true or false" in {
    assertResult(y)((x && y)(Set(x), Set()))
  }
  it should "result in False when x is true and y is in false" in {
    assertResult(FalseCondition)((x && y)(Set(x), Set(y)))
  }
  it should "result in False when x and y are in false" in {
    assertResult(FalseCondition)((x && y)(Set(), Set(x, y)))
  }

  "Applying a condition to a OR(x, y)" should "result in true when only one in true" in {
    assertResult(TrueCondition)((x || y)(Set(x)))
  }
  it should "result in false when neither in true" in {
    assertResult(FalseCondition)((x || y)(Set()))
  }
  it should "result in true when both in true" in {
    assertResult(TrueCondition)((x || y)(Set(x, y)))
  }
  it should "result in the OR(x, y) when neither in true or false" in {
    assertResult(OR(Set(x, y)))((x || y)(Set(), Set()))
  }
  it should "result in True when x is true and y is neither in true or false" in {
    assertResult(TrueCondition)((x || y)(Set(x), Set()))
  }
  it should "result in y when x is false and y is neither in true or false" in {
    assertResult(y)((x || y)(Set(), Set(x)))
  }
  it should "result in True when x is true and y is in false" in {
    assertResult(TrueCondition)((x || y)(Set(x), Set(y)))
  }
  it should "result in True when x and y are in false" in {
    assertResult(FalseCondition)((x || y)(Set(), Set(x, y)))
  }
}
