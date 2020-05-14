package booleanalgebra

sealed trait Condition {
  def &&(rhs: Condition): Condition = {
    (this, rhs) match {
      case (AND(conditionsL), AND(conditionsR)) => AND(conditionsL ++ conditionsR)
      case (AND(andConditions), a) => AND(andConditions + a)
      case (a, AND(andConditions)) => AND(andConditions + a)
      case (_, _) => AND(Set(this, rhs))
    }
  }
  def ||(rhs: Condition): Condition = {
    (this, rhs) match {
      case (OR(conditionsL), OR(conditionsR)) => OR(conditionsL ++ conditionsR)
      case (OR(orConditions), a) => OR(orConditions + a)
      case (a, OR(orConditions)) => OR(orConditions + a)
      case (_, _) => OR(Set(this, rhs))
    }
  }
  def unary_! : Condition = NOT(this)
  def simplify(normalForm: NormalForm = DNF): Condition
}

class Literal extends Condition {
  override def simplify(normalForm: NormalForm): Condition = {
    this
  }
}

case object TrueCondition extends Condition {
  override def &&(rhs: Condition): Condition = {
    rhs
  }
  override def ||(rhs: Condition): Condition = {
    this
  }

  override def simplify(normalForm: NormalForm): Condition = {
    this
  }
}

case object FalseCondition extends Condition {
  override def &&(rhs: Condition): Condition = {
    this
  }
  override def ||(rhs: Condition): Condition = {
    rhs
  }

  override def simplify(normalForm: NormalForm): Condition = {
    this
  }
}

case class AND(conditions: Set[Condition]) extends Condition {
  override def simplify(normalForm: NormalForm): Condition = {
    val resolvedConditions = conditions.map(_ simplify normalForm)
    resolvedConditions.foldLeft[Condition](FalseCondition)(normalForm.and)
  }

  override def toString = {
    conditions.map(_.toString).mkString("(", " && ", ")")
  }
}

case class OR(conditions: Set[Condition]) extends Condition {
  override def simplify(normalForm: NormalForm): Condition = {
    val resolvedConditions = conditions.map(_ simplify normalForm)
    resolvedConditions.foldLeft[Condition](TrueCondition)(normalForm.or)
  }

  override def toString = {
    conditions.map(_.toString).mkString("(", " || ", ")")
  }
}

case class NOT(condition: Condition) extends Condition {
  override def simplify(normalForm: NormalForm): Condition = {
    condition.simplify(normalForm) match {
      case AND(x) => OR(x.map(NOT))
      case OR(x) => AND(x.map(NOT))
      case NOT(x) => x
      case FalseCondition => TrueCondition
      case TrueCondition => FalseCondition
      case _: Literal => this
    }
  }

  override def toString = {
    s"!${condition}"
  }
}
