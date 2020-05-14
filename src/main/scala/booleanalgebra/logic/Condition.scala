package booleanalgebra.logic

import booleanalgebra.{DNF, NormalForm}
import booleanalgebra.Converter.boolToCondition

/**
 * Trait for Conditions
 */
sealed trait Condition {
  /**
   * @param rhs
   *            The condition to be ANDed with the Condition
   * @return
   *         The condition resulting from the AND
   */
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

  /**
   * @param rhs
   *            The condition to be ORed with the Condition
   * @return
   *         The condition resulting from the OR
   */
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

  /**
   * @return
   *         NOT(this)
   */
  def unary_! : Condition = {
    this match {
      case TrueCondition => false
      case FalseCondition => true
      case _ => NOT(this)
    }
  }

  /**
   * @param normalForm
   *                   The normal form to comply with
   * @return
   *         The resulting condition in the normal form
   */
  def simplify(normalForm: NormalForm = DNF): Condition

  /**
   * Applies Condition to a set of true literals. All others are false.
   * @param truths
   *               Set of Literal conditions that are true. All others are false.
   * @return
   *         The resulting condition. Will be TrueCondition or FalseCondition.
   */
  def apply(truths: Set[Literal]): Condition

  /**
   * Applies Condition to a set of true literals and set of false literals. All others remain undefined.
   * @param truths
   *               Set of Literal conditions that are true.
   * @param falses
   *               Set of Literal conditions that are false.
   * @return
   *         The resulting condition. Can be any condition.
   */
  def apply(truths: Set[Literal], falses: Set[Literal]): Condition
}

/**
 * A Literal, something that can be true or false but is not yet defined
 */
class Literal extends Condition {
  /**
   * @param normalForm
   *                   The normal form to comply with
   * @return
   *         Always returns the literal
   */
  override def simplify(normalForm: NormalForm): Condition = this

  /**
   * @param truths
   *               Set of Literal conditions that are true. All others are false.
   * @return
   *         The resulting condition. Will be TrueCondition if literal is in truths otherwise FalseCondition.
   */
  override def apply(truths: Set[Literal]): Condition = truths.contains(this)

  /**
   * @param truths
   *               Set of Literal conditions that are true.
   * @param falses
   *               Set of Literal conditions that are false.
   * @return
   *         The resulting condition. Will be TrueCondition if literal is in truths, FalseCondition if literal is in
   *         falses otherwise the original literal.
   */
  @throws(classOf[IllegalArgumentException])
  override def apply(truths: Set[Literal], falses: Set[Literal]): Condition = {
    (truths.contains(this), falses.contains(this)) match {
      case (false, false) => this
      case (true, false) => true
      case (false, true) => false
      case (true, true) => throw new IllegalArgumentException(s"Condition literal '${this}' cannot be both true and false")
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
    condition match {
      case AND(x) => OR(x.map(!_)) simplify normalForm
      case OR(x) => AND(x.map(!_)) simplify normalForm
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
