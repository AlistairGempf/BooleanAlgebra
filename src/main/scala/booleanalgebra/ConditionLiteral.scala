package booleanalgebra

import booleanalgebra.logic.Literal

trait ConditionLiteral extends Literal {
  val literal: Literal = new Literal()

  implicit def getLiteral: Literal = {
    literal
  }
}
