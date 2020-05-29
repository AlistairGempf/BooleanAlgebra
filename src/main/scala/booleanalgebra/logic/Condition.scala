package booleanalgebra.logic

import booleanalgebra.logic.DNF
import booleanalgebra.Converter.boolToCondition
import org.json4s.scalap.scalasig.ThisType

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

sealed trait Negation extends Condition {
  val condition: Condition

  override def apply(truths: Set[LiteralCondition]): Condition = NOT(condition(truths))
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition = NOT(condition(truths, falses))

  override def toString: String = s"!${condition}"
}

sealed trait DualOperator extends Condition {
  val conditions: Set[Condition]
  protected val identity: ResultCondition
  protected val annihilator: ResultCondition
  protected val operation: (Condition, Condition) => Condition

  override def normalise(normalForm: NormalForm): Condition = this.simplify match {
    case literal: Literal => literal
    case operator: DualOperator => operator.distribute(normalForm).simplify
  }

  def flatten: Condition
  def annihilate: Condition = if (conditions.contains(annihilator)) annihilator else this
  def filterIdentityFromConditions: Set[Condition] = conditions.filter(_ != identity)
  def filterIdentity: Condition
  def complement: Condition = if (conditions.count(a => conditions.contains(NOT(a))) > 0) annihilator else this
  def eliminate: Condition
  def absorb: Condition

  def isDual(condition: Condition): Either[DualOperator, Condition] = condition match {
    case c: DualOperator => Left(c)
    case c => Right(c)
  }

  override def apply(truths: Set[LiteralCondition]): Condition =
    conditions.foldLeft[Condition](identity)((a, b) => operation(a(truths), b(truths)))
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition =
    conditions.foldLeft[Condition](identity)((a, b) => operation(a(truths, falses), b(truths, falses)) )
}

case object TrueCondition extends ResultCondition {
  override def apply(truths: Set[LiteralCondition]): Condition = true
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition = true
}

case object FalseCondition extends ResultCondition {
  override def apply(truths: Set[LiteralCondition]): Condition = false
  override def apply(truths: Set[LiteralCondition], falses: Set[LiteralCondition]): Condition = false
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

case class NOTLiteral(condition: Literal) extends Negation with Literal

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

case class AND(conditions: Set[Condition]) extends DualOperator {
  protected val identity: ResultCondition = TrueCondition
  protected val annihilator: ResultCondition = FalseCondition
  protected val operation: (Condition, Condition) => Condition = (lhs, rhs) => lhs && rhs

  override def flatten: DualOperator = {
    AND(conditions.flatMap(_ match {
      case AND(internalConditions) => internalConditions
      case a => Set(a)
    }))
  }
  def filterIdentity: Condition = {
    val filtered = filterIdentityFromConditions
    if (filtered.isEmpty)  {
      identity
    } else {
      AND(filterIdentityFromConditions)
    }
  }
  override def eliminate: Condition = {
    val resultSet = conditions.foldLeft(Set[Condition]())((conditionSet, condition) => {
      condition match {
        case OR(orConditions) => {
          val setOfOtherNots = orConditions.flatMap(c => Map[Condition, Set[Condition]](c -> (orConditions.filter(_ != c).map(NOT(_)) + c)))
          val contained = setOfOtherNots.filter((nottedConditions) => conditionSet.contains(OR(nottedConditions._2)))
          if (contained.isEmpty) {
            conditionSet + OR(orConditions)
          } else {
            val removedCondition = conditionSet.filter(_ != OR(contained.head._2))
            removedCondition + contained.head._1
          }
        }
        case a => conditionSet + a
      }
    })
    if (resultSet.size == 1) {
      resultSet.head
    } else {
      AND(resultSet)
    }
  }
  private def positiveAbsorption = (originalConditionSet: Set[Condition]) =>
    originalConditionSet.foldLeft(Set[Condition]())((conditionSet, condition) => {
      condition match {
        case OR(orConditions) => {
          val contained = orConditions intersect conditionSet
          val resultConditionSet = if (contained.isEmpty) {
            conditionSet + OR(orConditions)
          } else {
            conditionSet
          }
          resultConditionSet
        }
        case a => {
          val matchingOrsRemoved = conditionSet.filter(_ match {
            case OR(orConditions) if (orConditions.contains(a)) => false
            case _ => true
          })
          matchingOrsRemoved + a
        }
      }
    })
  private def negativeAbsorption = (originalConditionSet: Set[Condition]) =>
    originalConditionSet.foldLeft(Set[Condition]())((conditionSet, condition) => {
      condition match {
        case OR(orConditions) => {
          val simplifyOrSet: Set[Condition] => Set[Condition] = orSet => if (orSet.size < 2) orSet else Set(OR(orSet))
          val matchingConditions: Set[Condition] = orConditions.filter(a => conditionSet.contains(NOT(a)))
          val nonMatchingConditions: Set[Condition] = orConditions.filter(a => !conditionSet.contains(NOT(a)))
          conditionSet.diff(matchingConditions) ++ simplifyOrSet(matchingConditions.map(NOT(_))) ++ simplifyOrSet(nonMatchingConditions)
        }
        case a => {
          val resultConditionSet: Set[Condition] = conditionSet.flatMap {
            case OR(orConditions) => orConditions.filter(_ != NOT(a))
            case b => Set(b)
          }
          resultConditionSet + a
        }
      }
    })
  override def absorb: Condition = {
    val positiveAbsorb = positiveAbsorption(conditions)
    val resultSet = negativeAbsorption(positiveAbsorb)
    if (resultSet.size == 1) {
      resultSet.head
    } else if (resultSet.isEmpty) {
      throw new RuntimeException("Result of absorption should never be no elements")
    } else {
      AND(resultSet)
    }
  }

  override def literalise: Condition = AND(this.conditions.map(_.literalise)).flatten
  override def distribute(normalForm: NormalForm): Condition = conditions.foldLeft[Condition](identity)(normalForm.and)
  override def simplify: Condition = {
    val annihilated = AND(conditions.map(_.simplify)).flatten.annihilate
    isDual(annihilated) match {
      case Left(value) => isDual(value.filterIdentity) match {
        case Left(value) => isDual(value.complement) match {
          case Left(value) => isDual(value.eliminate) match {
            case Left(value) => isDual(value.absorb) match {
              case Left(value) => value.complement
              case Right(value) => value
            }
            case Right(value) => value
          }
          case Right(value) => value
        }
        case Right(value) => value
      }
      case Right(value) => value
    }
  }

  override def toString: String = conditions.mkString("( ", " && ", " )")
}

case class OR(conditions: Set[Condition]) extends DualOperator {
  protected val identity: ResultCondition = FalseCondition
  protected val annihilator: ResultCondition = TrueCondition
  protected val operation: (Condition, Condition) => Condition = (lhs, rhs) => lhs || rhs

  override def flatten: DualOperator = {
    OR(conditions.flatMap(_ match {
      case OR(internalConditions) => internalConditions
      case a => Set(a)
    }))
  }
  def filterIdentity: Condition = {
    val filtered = filterIdentityFromConditions
    if (filtered.isEmpty)  {
      identity
    } else {
      OR(filterIdentityFromConditions)
    }
  }
  override def eliminate: Condition = {
    val resultSet = conditions.foldLeft(Set[Condition]())((conditionSet, condition) => {
      condition match {
        case AND(andConditions) => {
          val setOfOtherNots = andConditions.flatMap(c => Map[Condition, Set[Condition]](c -> (andConditions.filter(_ != c).map(NOT(_)) + c)))
          val contained = setOfOtherNots.filter((nottedConditions) => conditionSet.contains(AND(nottedConditions._2)))
          if (contained.isEmpty) {
            conditionSet + AND(andConditions)
          } else {
            val removedCondition = conditionSet.filter(_ != AND(contained.head._2))
            removedCondition + contained.head._1
          }
        }
        case a => conditionSet + a
      }
    })
    if (resultSet.size == 1) {
      resultSet.head
    } else if (resultSet.isEmpty) {
      throw new RuntimeException("Result of elimination should never be no elements")
    } else {
      OR(resultSet)
    }
  }
  private def positiveAbsorption = (originalConditionSet: Set[Condition]) =>
    originalConditionSet.foldLeft(Set[Condition]())((conditionSet, condition) => {
      condition match {
        case AND(andConditions) => {
          val contained = andConditions intersect conditionSet
          if (contained.isEmpty) {
            conditionSet + AND(andConditions)
          } else {
            conditionSet
          }
        }
        case a => {
          val matchingAndsRemoved = conditionSet.filter(_ match {
            case AND(andConditions) => if (andConditions.contains(a)) false else true
            case _ => true
          })
          matchingAndsRemoved + a
        }
      }
    })
  private def negativeAbsorption = (originalConditionSet: Set[Condition]) =>
    originalConditionSet.foldLeft(Set[Condition]())((conditionSet, condition) => {
      condition match {
          // (~A & B) | A = A | B
        case AND(andConditions) => {
          val simplifyAndSet: Set[Condition] => Set[Condition] = andSet => if (andSet.size < 2) andSet else Set(AND(andSet))
          val matchingAndConditions: Set[Condition] = andConditions.filter(a => conditionSet.contains(NOT(a)))
          val nonMatchingAndConditions: Set[Condition] = andConditions.filter(a => !conditionSet.contains(NOT(a)))
          conditionSet.diff(matchingAndConditions) ++ simplifyAndSet(matchingAndConditions.map(NOT(_))) ++ simplifyAndSet(nonMatchingAndConditions)
        }
        case a => {
          val resultConditionSet: Set[Condition] = conditionSet.flatMap {
            case AND(andConditions) => andConditions.filter(_ != NOT(a))
            case b => Set(b)
          }
          resultConditionSet + a
        }
      }
    })
  override def absorb: Condition = {
    val positiveAbsorb = positiveAbsorption(conditions)
    val resultSet = negativeAbsorption(positiveAbsorb)
    if (resultSet.size == 1) {
      resultSet.head
    } else if (resultSet.isEmpty) {
      throw new RuntimeException("Result of absorption should never be no elements")
    } else {
      OR(resultSet)
    }
  }

  override def literalise: Condition = OR(this.conditions.map(_.literalise)).flatten
  override def distribute(normalForm: NormalForm): Condition = conditions.foldLeft[Condition](identity)(normalForm.or)
  override def simplify: Condition = {
    val annihilated = OR(conditions.map(_.simplify)).flatten.annihilate
    isDual(annihilated) match {
      case Left(value) => isDual(value.filterIdentity) match {
        case Left(value) => isDual(value.complement) match {
          case Left(value) => isDual(value.eliminate) match {
            case Left(value) => isDual(value.absorb) match {
              case Left(value) => value.complement
              case Right(value) => value
            }
            case Right(value) => value
          }
          case Right(value) => value
        }
        case Right(value) => value
      }
      case Right(value) => value
    }
  }

  override def toString: String = conditions.mkString("( ", " || ", " )")
}
