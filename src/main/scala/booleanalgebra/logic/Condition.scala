package booleanalgebra.logic

import booleanalgebra.logic.DNF
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

  def normalise(normalForm: NormalForm): Condition

  def literalise: Condition

  def distribute(normalForm: NormalForm): Condition

  /**
   * @return
   *         The simplified condition in the normal form
   */
  def simplify: Condition

  /**
   * Applies Condition to a set of true literals. All others are false.
   * @param truths
   *               Set of Literal conditions that are true. All others are false.
   * @return
   *         The resulting condition. Will be TrueCondition or FalseCondition.
   */
  def apply(truths: Set[LiteralCondition]): Condition

  /**
   * Applies Condition to a set of true literals and set of false literals. All others remain undefined.
   * @param truths
   *               Set of Literal conditions that are true.
   * @param falses
   *               Set of Literal conditions that are false.
   * @return
   *         The resulting condition. Can be any condition.
   */
  def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition
}

sealed trait Literal extends Condition {
  override def normalise(normalForm: NormalForm): Condition = this
  override def literalise: Condition = this
  override def distribute(normalForm: NormalForm): Condition = this
  /**
   * @return
   * The simplified condition in the normal form
   */
  override def simplify: Condition = this
}

/**
 * A Literal, something that can be true or false but is not yet defined
 */
class LiteralCondition extends Literal {
  /**
   * @param truths
   *               Set of Literal conditions that are true. All others are false.
   * @return
   *         The resulting condition. Will be TrueCondition if literal is in truths otherwise FalseCondition.
   */
  override def apply(truths: Set[LiteralCondition]): Condition = truths.contains(this)

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
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition = {
    (truths.contains(this), falses.contains(this)) match {
      case (false, false) => this
      case (true, false) => true
      case (false, true) => false
      case (true, true) => throw new IllegalArgumentException(s"Condition literal '${this}' cannot be both true and false")
    }
  }
}

sealed trait ResultCondition extends Literal {
  override def normalise(normalForm: NormalForm): Condition = this
  override def literalise: Condition = this
  override def distribute(normalForm: NormalForm): Condition = this
  /**
   * @return
   * The simplified condition in the normal form
   */
  override def simplify: Condition = this
}

case object TrueCondition extends ResultCondition {
  override def apply(truths: Set[LiteralCondition]): Condition = true
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition = true
}

case object FalseCondition extends ResultCondition {
  override def apply(truths: Set[LiteralCondition]): Condition = false
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition = false
}

sealed trait DualOperator extends Condition {
  val conditions: Set[Condition]
  val identity: ResultCondition
  val annihilator: ResultCondition
  val operation: (Condition, Condition) => Condition

  override def apply(truths: Set[LiteralCondition]): Condition =
    conditions.foldLeft[Condition](identity)((a, b) => operation(a(truths), b(truths)))
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition =
    conditions.foldLeft[Condition](identity)((a, b) => operation(a(truths, falses), b(truths, falses)) )
}

case class AND(conditions: Set[Condition]) extends DualOperator {
  val identity: ResultCondition = TrueCondition
  val annihilator: ResultCondition = FalseCondition
  val operation: (Condition, Condition) => Condition = (lhs, rhs) => lhs && rhs

  override def normalise(normalForm: NormalForm): Condition = this

  override def literalise: Condition = AND(this.conditions.map(_.literalise))
  override def distribute(normalForm: NormalForm): Condition = this
  override def simplify: Condition = this
}

case class OR(conditions: Set[Condition]) extends DualOperator {
  override val identity: ResultCondition = FalseCondition
  override val annihilator: ResultCondition = TrueCondition
  val operation: (Condition, Condition) => Condition = (lhs, rhs) => lhs || rhs

  override def normalise(normalForm: NormalForm): Condition = this

  override def literalise: Condition = OR(this.conditions.map(_.literalise))
  override def distribute(normalForm: NormalForm): Condition = this
  override def simplify: Condition = this
}

sealed trait Negation extends Condition {
  val condition: Condition

  override def apply(truths: Set[LiteralCondition]): Condition = NOT(condition(truths))
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition = NOT(condition(truths, falses))
}

case class NOTCondition(condition: DualOperator) extends Negation {
  override def literalise: Condition = {
    condition match {
      case AND(conditions) => OR(conditions.map(NOT(_))).literalise
      case OR(conditions) => AND(conditions.map(NOT(_))).literalise
    }
  }

  override def normalise(normalForm: NormalForm): Condition = condition.literalise.normalise(normalForm)
  override def distribute(normalForm: NormalForm): Condition = condition.literalise.distribute(normalForm)
  override def simplify: Condition = this
}

case class NOTLiteral(condition: Literal) extends Negation with Literal

object NOT {
  def apply(condition: Condition): Condition = {
    condition match {
      case resultCondition: ResultCondition => resultCondition match {
        case TrueCondition => FalseCondition
        case FalseCondition => TrueCondition
      }
      case negation: Negation => negation.condition
      case literal: Literal => NOTLiteral(literal)
      case operator: DualOperator => NOTCondition(operator)
    }
  }
}
