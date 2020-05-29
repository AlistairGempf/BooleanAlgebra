import booleanalgebra.ConditionLiteral
import booleanalgebra.Converter._
import booleanalgebra.logic.DNF

case class PredicateCondition(predicate: String, conditionObject: String) extends ConditionLiteral {
  override def toString = {
    s"${predicate} ${conditionObject}"
  }
}

object BooleanAlgebra extends App {
  val a = PredicateCondition("about", "a")
  val b = PredicateCondition("about", "b")
  val b2 = PredicateCondition("about", "b")
  val c = PredicateCondition("about", "c")
  val d = PredicateCondition("about", "d")
  val e = PredicateCondition("about", "e")
  val mend = PredicateCondition("mentions", "d")
  val mene = PredicateCondition("mentions", "e")
  val f = PredicateCondition("about", "f")
  val g = PredicateCondition("about", "g")
  val h = PredicateCondition("about", "h")
  val ha = PredicateCondition("top", "ha")
  val english = PredicateCondition("language", "en-gb")
  val welsh = PredicateCondition("language", "cy")
  println("Hello Boolean Algebra")

  println(((a || b) && (c || d)).normalise(DNF))
}
