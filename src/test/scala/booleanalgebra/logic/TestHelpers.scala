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
}
