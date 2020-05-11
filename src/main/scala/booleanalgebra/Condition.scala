package booleanalgebra

sealed trait Condition {
  def &&(rhs: Condition): Condition = {
    AND(Set(this, rhs))
  }
  def ||(rhs: Condition): Condition = {
    OR(Set(this, rhs))
  }
  def resolve: Condition
  def unary_! = NOT(this)
}

case class PredicateCondition(predicate: String, conditionObject: String) extends Condition {
  override def resolve: Condition = {
    this
  }

  override def toString = {
    s"${predicate} ${conditionObject}"
  }
}

case class TrueCondition() extends Condition {
  override def &&(rhs: Condition): Condition = {
    rhs
  }
  override def ||(rhs: Condition): Condition = {
    this
  }

  override def resolve: Condition = {
    this
  }
}

case class FalseCondition() extends Condition {
  override def &&(rhs: Condition): Condition = {
    this
  }
  override def ||(rhs: Condition): Condition = {
    rhs
  }

  override def resolve: Condition = {
    this
  }
}

case class AND(conditions: Set[Condition]) extends Condition {
  override def resolve: Condition = {
    if (conditions.isEmpty) {
      return TrueCondition()
    }
    if (conditions.contains(FalseCondition())) {
      FalseCondition()
    } else {
      val resolvedConditions = conditions.filter(_ != FalseCondition()).filter(_ != TrueCondition()).map(_.resolve)

      val flattenAnds: Set[Condition] = resolvedConditions.flatMap {
        case AND(ands) => ands
        case condition => List(condition)
      }
      if (flattenAnds.exists(condition => flattenAnds.contains(NOT(condition).resolve))) {
        return FalseCondition()
      }

      val theOrs = resolvedConditions.filter(_ match {
        case _: OR => true
        case _ => false
      })

      val notOrs = resolvedConditions.flatMap {
        case _: OR => List()
        case condition => List(condition)
      }

      val multipliedOrs = theOrs.foldRight[Condition](FalseCondition())((condition, b) => {
        (condition, b) match {
          case (OR(conditionsL), OR(conditionsR)) => OR(conditionsL.flatMap(left => conditionsR.map(right => left && right))).resolve
          case (FalseCondition(), otherCondition) => otherCondition
          case (otherCondition, FalseCondition()) => otherCondition
        }
      })

      val multipliedOrConditions = multipliedOrs match {
        case OR(conditions) => conditions
        case _ => List()
      }

      val multipliedOut = notOrs.flatMap(and => multipliedOrConditions.map(or => AND(Set(and, or))))

      (multipliedOut.size, flattenAnds.size, notOrs.size, conditions.contains(FalseCondition())) match {
        case (_, _, _, true) => FalseCondition()
        case (0, 1, _, false) => flattenAnds.head
        case (0, _, 0, false) => multipliedOrs
        case (0, _, _, false) => AND(flattenAnds)
        case _ => OR(multipliedOut.toSet).resolve
      }
    }
  }

  override def toString = {
    conditions.map(_.toString).mkString("(", " && ", ")")
  }
}

case class OR(conditions: Set[Condition]) extends Condition {
  override def resolve: Condition = {
    if (conditions.isEmpty) {
      return TrueCondition()
    }
    if (conditions.contains(TrueCondition())) {
      TrueCondition()
    } else {
      val resolvedConditions = conditions.map(_.resolve)
      if (resolvedConditions.exists(condition => resolvedConditions.contains(NOT(condition).resolve))) {
        return TrueCondition()
      }
      val flattenOrs: Set[Condition] = resolvedConditions.flatMap {
        case OR(ors) => ors
        case condition => List(condition)
      }

      (conditions.contains(TrueCondition()), flattenOrs.size) match {
        case (true, _) => TrueCondition()
        case (false, 1) => flattenOrs.head.resolve
        case (false, _) => OR(flattenOrs)
      }
    }
  }

  override def toString = {
    conditions.map(_.toString).mkString("(", " || ", ")")
  }
}

case class NOT(condition: Condition) extends Condition {
  override def resolve: Condition = {
    condition.resolve match {
      case AND(x) => OR(x.map(NOT))
      case OR(x) => AND(x.map(NOT))
      case NOT(x) => x
      case FalseCondition() => TrueCondition()
      case TrueCondition() => FalseCondition()
      case _: PredicateCondition => this
    }
  }

  override def toString = {
    s"!${condition}"
  }
}
