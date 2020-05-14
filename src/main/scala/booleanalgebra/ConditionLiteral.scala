package booleanalgebra

trait ConditionLiteral extends Literal {
  val literal: Literal = new Literal()

  implicit def getLiteral: Literal = {
    literal
  }
}
