package booleanalgebra

import booleanalgebra.logic.LiteralCondition

trait ConditionLiteral extends LiteralCondition {
  val literal: LiteralCondition = new LiteralCondition()

  implicit def getLiteral: LiteralCondition = {
    literal
  }
}
