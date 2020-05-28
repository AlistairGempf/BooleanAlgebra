package booleanalgebra.logic

trait NormalForm {
  def and(lhs: Condition, rhs: Condition): Condition
  def or(lhs: Condition, rhs: Condition): Condition
}
case object CNF extends NormalForm {
  override def and(lhs: Condition, rhs: Condition): Condition = {
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
      case (or: OR, AND(andConditions)) => AND(andConditions.map(or || _))
      case (AND(andConditions), or: OR) => AND(andConditions.map(_ || or))
      case (AND(conditionsL), AND(conditionsR)) => AND(conditionsL.flatMap(l => conditionsR.map(l || _)))
      case(literal, AND(andConditions)) => AND(andConditions.map(literal || _))
      case(AND(andConditions), literal) => AND(andConditions.map(_ || literal))
      case (a, b) => a || b
    }
  }
}

case object DNF extends NormalForm {

  override def and(lhs: Condition, rhs: Condition): Condition = {
    (lhs, rhs) match {
        // A & 0 = 0
      case (_, FalseCondition) => FalseCondition
      case (FalseCondition, _) => FalseCondition
        // A & 1 = A
      case (a, TrueCondition) => a
      case (TrueCondition, a) => a
        // (a || b) && (c && d) = (a && c && d) || (b && c && d)
      case (OR(orConditions), and: AND) => OR(orConditions.map(_ && and))
      case (and: AND, OR(orConditions)) => OR(orConditions.map(and && _))
        // (a || b) && (c || d) = (a && c) || (a && d) || (b && c) || (b && d)
      case (OR(conditionsL), OR(conditionsR)) => OR(conditionsL.flatMap(l => conditionsR.map(l && _)))
        // a && (b || c) = (a && b) || (a && c)
      case(literal, OR(orConditions)) => OR(orConditions.map(literal && _))
      case(OR(orConditions), literal) => OR(orConditions.map(_ && literal))
        // Default
      case (a, b) => a && b
    }
  }

  override def or(lhs: Condition, rhs: Condition): Condition = {
    (lhs, rhs) match {
        // A | 1 = 1
      case (_, TrueCondition) => TrueCondition
      case (TrueCondition, _) => TrueCondition
        // A | 0 = A
      case (a, FalseCondition) => a
      case (FalseCondition, a) => a
        // (a || b) || (c || d) = a || b || c || d
      case (OR(conditionsL), OR(conditionsR)) => OR(conditionsL ++ conditionsR)
        // (a && b) || (c || d) = (a && b) || c || d
      case (and: AND, OR(orConditions)) => OR(orConditions + and)
      case (OR(orConditions), and: AND) => OR(orConditions + and)
        // default
      case (a, b) => a || b
    }
  }
}
