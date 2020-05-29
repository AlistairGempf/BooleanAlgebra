package booleanalgebra.logic

import booleanalgebra.ConditionLiteral
import scala.util.Random

object TestHelpers {
  case class ConditionObject(conditionObject: String) extends ConditionLiteral {
    override def toString = {
      s"${conditionObject}"
    }
  }
  val a = ConditionObject("a")
  val b = ConditionObject("b")
  val c = ConditionObject("c")
  val d = ConditionObject("d")
  val e = ConditionObject("e")
  val f = ConditionObject("f")
  val g = ConditionObject("g")
  val h = ConditionObject("h")
  val i = ConditionObject("i")
  val j = ConditionObject("j")
  val k = ConditionObject("k")
  val l = ConditionObject("l")
  val m = ConditionObject("m")
  val n = ConditionObject("n")
  val o = ConditionObject("o")
  val p = ConditionObject("p")
  val q = ConditionObject("q")
  val r = ConditionObject("r")
  val s = ConditionObject("s")
  val t = ConditionObject("t")
  val u = ConditionObject("u")
  val v = ConditionObject("v")
  val w = ConditionObject("w")
  val x = ConditionObject("x")
  val y = ConditionObject("y")
  val z = ConditionObject("z")
  val conditions = Set(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
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
  val operations = Set("AND", "NOT", "OR")
  val rnd = new Random
  def createTruthTable(condition: Condition): Set[Tuple2[Set[LiteralCondition], Set[LiteralCondition]]] = {
    createTruthTable(getLiterals(condition))
  }
  def createTruthTable(literals: Set[LiteralCondition]): Set[Tuple2[Set[LiteralCondition], Set[LiteralCondition]]] = {
    literals.size match {
      case 0 => throw new RuntimeException()
      case 1 => Set((Set(literals.head), Set()), (Set(), Set(literals.head)))
      case _ => createTruthTable(literals.tail).flatMap(a => Set((a._1 + literals.head, a._2), (a._1, a._2 + literals.head)))
    }
  }
  def getLiterals(condition: Condition): Set[LiteralCondition] = {
    condition match {
      case literalCondition: LiteralCondition => Set(literalCondition)
      case negation: Negation => getLiterals(negation.condition)
      case dualOperator: DualOperator => dualOperator.conditions.flatMap(getLiterals)
    }
  }

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
