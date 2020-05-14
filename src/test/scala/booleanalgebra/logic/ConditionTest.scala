package booleanalgebra.logic
import org.scalatest._

class ConditionTest extends FlatSpec {
  val literal1 = new Literal
  val literal2 = new Literal
  "True && False" should "give False" in {
    assertResult(FalseCondition)(TrueCondition && FalseCondition)
  }
  "False && False" should "give False" in {
    assertResult(FalseCondition)(FalseCondition && FalseCondition)
  }
  "True && True" should "give True" in {
    assertResult(TrueCondition)(TrueCondition && TrueCondition)
  }
  "True && (AND(x, y))" should "give AND(x, y)" in {
    assertResult(AND(Set(literal1, literal2)))(TrueCondition && AND(Set(literal1, literal2)))
  }
  "False && (AND(x, y))" should "give False" in {
    assertResult(FalseCondition)(FalseCondition && AND(Set(literal1, literal2)))
  }
}
