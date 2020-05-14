package booleanalgebra

import booleanalgebra.logic.{AND, Condition, FalseCondition, OR, TrueCondition}

trait NormalForm {
  def and(lhs: Condition, rhs: Condition): Condition
  def or(lhs: Condition, rhs: Condition): Condition
}
case object CNF extends NormalForm {
  def and(lhs: Condition, rhs: Condition): Condition = {
    (lhs, rhs) match {
      case (_, TrueCondition) => TrueCondition
      case (TrueCondition, _) => TrueCondition
      case (a, FalseCondition) => a
      case (FalseCondition, a) => a
      case (AND(conditionsL), AND(conditionsR)) => AND(conditionsL ++ conditionsR)
      case (AND(conditionsL), or: OR) => AND(conditionsL + or)
      case (or: OR, AND(conditionsR)) => AND(conditionsR + or)
      case (a, b) => a && b
    }
  }

  override def or(lhs: Condition, rhs: Condition): Condition = {
    (lhs, rhs) match {
      case (_, FalseCondition) => FalseCondition
      case (FalseCondition, _) => FalseCondition
      case (a, TrueCondition) => a
      case (TrueCondition, a) => a
      case (or, AND(andConditions)) => AND(andConditions.map(or || _))
      case (AND(andConditions), or) => AND(andConditions.map(_ || or))
      case (AND(conditionsL), AND(conditionsR)) => AND(conditionsL.flatMap(l => conditionsR.map(l || _)))
      case(literal, AND(andConditions)) => AND(andConditions.map(literal || _))
      case(AND(andConditions), literal) => AND(andConditions.map(_ || literal))
      case (a, b) => a || b
    }
  }
}

case object DNF extends NormalForm {

  def and(lhs: Condition, rhs: Condition): Condition = {
    (lhs, rhs) match {
      case (_, TrueCondition) => TrueCondition
      case (TrueCondition, _) => TrueCondition
      case (a, FalseCondition) => a
      case (FalseCondition, a) => a
      case (OR(orConditions), and) => OR(orConditions.map(_ && and))
      case (and, OR(orConditions)) => OR(orConditions.map(and && _))
      case (OR(conditionsL), OR(conditionsR)) => OR(conditionsL.flatMap(l => conditionsR.map(l && _)))
      case(literal, OR(orConditions)) => OR(orConditions.map(literal && _))
      case(OR(orConditions), literal) => OR(orConditions.map(_ && literal))
      case (a, b) => a && b
    }
  }

  override def or(lhs: Condition, rhs: Condition): Condition = {
    (lhs, rhs) match {
      case (_, FalseCondition) => FalseCondition
      case (FalseCondition, _) => FalseCondition
      case (a, TrueCondition) => a
      case (TrueCondition, a) => a
      case (OR(conditionsL), OR(conditionsR)) => OR(conditionsL ++ conditionsR)
      case (and: AND, OR(orConditions)) => OR(orConditions + and)
      case (OR(orConditions), and: AND) => OR(orConditions + and)
      case (a, b) => a || b
    }
  }
}
