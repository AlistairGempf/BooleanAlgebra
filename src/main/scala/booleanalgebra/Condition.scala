package booleanalgebra

import Converter.boolToCondition

sealed trait Condition {
  def &&(rhs: Condition): Condition = {
    (this, rhs) match {
        //  False && a = False
      case (FalseCondition, _) => FalseCondition
      case (_, FalseCondition) => FalseCondition
        //  True && a = a
      case (TrueCondition, a) => a
      case (a, TrueCondition) => a
        //  (a && b) && (c && d) = a && b && c && d
      case (AND(conditionsL), AND(conditionsR)) => AND(conditionsL ++ conditionsR)
        //  (a && b) && c = a && b && c
      case (AND(andConditions), a) => AND(andConditions + a)
      case (a, AND(andConditions)) => AND(andConditions + a)
        //  Stick anything else inside the AND to be resolved if we want to simplify
      case (_, _) => AND(Set(this, rhs))
    }
  }
  def ||(rhs: Condition): Condition = {
    (this, rhs) match {
        // True || a = True
      case (TrueCondition, _) | (_, TrueCondition) => TrueCondition
        // False || a = a
      case (FalseCondition, a) => a
      case (a, FalseCondition) => a
        // (a || b) || (c || d) = a || b || c || d
      case (OR(conditionsL), OR(conditionsR)) => OR(conditionsL ++ conditionsR)
        // ((a || b) || c) = a || b || c
      case (OR(orConditions), a) => OR(orConditions + a)
      case (a, OR(orConditions)) => OR(orConditions + a)
        // Stick anything else inside the OR to be resolved if we want to simplify
      case (_, _) => OR(Set(this, rhs))
    }
  }
  def unary_! : Condition = NOT(this)

    //  Simplify to a specific form
  def simplify(normalForm: NormalForm = DNF): Condition

    //  Apply the conditions to a set of truths (everything else is false)
  def apply(truths: Set[Literal]): Condition
    //  Apply the conditions to a set of truths and falses (everything else is undefined)
  def apply(truths: Set[Literal], falses: Set[Literal]): Condition
}

class Literal extends Condition {
  override def simplify(normalForm: NormalForm): Condition = {
    this
  }

  override def apply(truths: Set[Literal]): Condition = {
    truths.contains(this)
  }

  override def apply(truths: Set[Literal], falses: Set[Literal]): Condition = {
    (truths.contains(this), falses.contains(this)) match {
      case (false, false) => this
      case (true, false) => true
      case (false, true) => false
    }
  }
}

case object TrueCondition extends Condition {
  override def simplify(normalForm: NormalForm): Condition = true
  override def apply(truths: Set[Literal]): Condition = true
  override def apply(truths: Set[Literal], falses: Set[Literal]): Condition = true
}

case object FalseCondition extends Condition {
  override def simplify(normalForm: NormalForm): Condition = false
  override def apply(truths: Set[Literal]): Condition = false
  override def apply(truths: Set[Literal], falses: Set[Literal]): Condition = false
}

case class AND(conditions: Set[Condition]) extends Condition {
  override def simplify(normalForm: NormalForm): Condition = {
    val resolvedConditions = conditions.map(_ simplify normalForm)
    resolvedConditions.foldLeft[Condition](FalseCondition)(normalForm.and)
  }

  override def toString: String = {
    conditions.map(_.toString).mkString("(", " && ", ")")
  }

  override def apply(truths: Set[Literal]): Condition = {
    conditions.foldLeft[Condition](TrueCondition)(_(truths) && _(truths))
  }

  override def apply(truths: Set[Literal], falses: Set[Literal]): Condition = {
    conditions.foldLeft[Condition](TrueCondition)(_(truths, falses) && _(truths, falses))
  }
}

case class OR(conditions: Set[Condition]) extends Condition {
  override def simplify(normalForm: NormalForm): Condition = {
    val resolvedConditions = conditions.map(_ simplify normalForm)
    resolvedConditions.foldLeft[Condition](TrueCondition)(normalForm.or)
  }

  override def toString: String = {
    conditions.map(_.toString).mkString("(", " || ", ")")
  }

  override def apply(truths: Set[Literal]): Condition = {
    conditions.foldLeft[Condition](FalseCondition)(_(truths) || _(truths))
  }

  override def apply(truths: Set[Literal], falses: Set[Literal]): Condition = {
    conditions.foldLeft[Condition](FalseCondition)(_(truths, falses) || _(truths, falses))
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

  override def toString: String = {
    s"!$condition"
  }

  override def apply(truths: Set[Literal]): Condition = {
    !condition(truths)
  }

  override def apply(truths: Set[Literal], falses: Set[Literal]): Condition = {
    !condition(truths, falses)
  }
}
