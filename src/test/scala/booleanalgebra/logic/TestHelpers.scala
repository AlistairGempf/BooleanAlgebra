package booleanalgebra.logic

import booleanalgebra.ConditionLiteral

object TestHelpers {
  case class ConditionObject(conditionObject: String) extends ConditionLiteral {
    override def toString = {
      s"${conditionObject}"
    }
  }
  val w = ConditionObject("w")
  val x = ConditionObject("x")
  val y = ConditionObject("y")
  val z = ConditionObject("z")
  val conditionList = Set(w, x, y, z)
  val truthTable: Set[Tuple2[Set[LiteralCondition], Set[LiteralCondition]]] = Set(
    (Set(), Set(w, x, y, z)),
    (Set(w), Set(x, y, z)),
    (Set(x), Set(w, y, z)),
    (Set(w, x), Set(y, z)),
    (Set(y), Set(w, x, z)),
    (Set(w, y), Set(x, z)),
    (Set(x, y), Set(w, z)),
    (Set(w, x, y), Set(z)),
    (Set(z), Set(w, x, y)),
    (Set(w, z), Set(x, y)),
    (Set(x, z), Set(w, y)),
    (Set(w, x, z), Set(y)),
    (Set(y, z), Set(w, x)),
    (Set(w, y, z), Set(x)),
    (Set(x, y, z), Set(w)),
    (Set(w, x, y, z), Set()),
  )
  implicit def conditionToFormChecker(condition: Condition): FormChecker = {
    FormChecker(condition)
  }
  case class FormChecker(condition: Condition) {
    def isDNF: Boolean = {
      val bools: Set[Boolean] = condition match {
        case _: Literal => Set(true)
        case OR(orCondition) => orCondition.flatMap {
          case _: Literal => Set(true)
          case AND(andCondtions) => andCondtions.map {
            case _: Literal => true
            case _ => false
          }
          case _ => Set(false)
        }
        case AND(andCondition) => andCondition.map {
          case _: Literal => true
          case _ => false
        }
        case _: NOTCondition => Set(false)
      }
      !bools.contains(false)
    }
    def isCNF: Boolean = {
      val bools: Set[Boolean] = condition match {
        case _: Literal => Set(true)
        case AND(orCondition) => orCondition.flatMap {
          case _: Literal => Set(true)
          case OR(orCondtions) => orCondtions.map {
            case _: Literal => true
            case _ => false
          }
          case _ => Set(false)
        }
        case OR(orCondition) => orCondition.map {
          case _: Literal => true
          case _ => false
        }
        case _: NOTCondition => Set(false)
      }
      !bools.contains(false)
    }
  }
}
