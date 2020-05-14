import booleanalgebra.{CNF, DNF, ConditionLiteral}
import booleanalgebra.Converter._

case class PredicateCondition(predicate: String, conditionObject: String) extends ConditionLiteral {
  override def toString = {
    s"${predicate} ${conditionObject}"
  }
}

object BooleanAlgebra extends App {
  val a = PredicateCondition("about", "a")
  val b = PredicateCondition("about", "b")
  val c = PredicateCondition("about", "c")
  val d = PredicateCondition("about", "d")
  val e = PredicateCondition("about", "e")
  val mend = PredicateCondition("mentions", "d")
  val mene = PredicateCondition("mentions", "e")
  val f = PredicateCondition("about", "f")
  val g = PredicateCondition("about", "g")
  val ha = PredicateCondition("top", "ha")
  val english = PredicateCondition("language", "en-gb")
  val welsh = PredicateCondition("language", "cy")
  println("Hello Boolean Algebra")
//  println((a && b && c).resolve)
//  println((a || b || c).resolve)
//  println((a && b && (c || d)).resolve)
//  println(((a && b) && (c || (d || (e && (f || g))))).resolve)
//  println((a && !(a)).resolve)
//  println((ha && OR(Set(AND(Set(OR(Set(a && b && (c || d)))))))).resolve)
//  println(!(a).resolve)
//  println((!(!a)).resolve)
//  println(!(a || b).resolve)
//  println(!(a && b).resolve)
//  println((a && b && !(c || d)).resolve)
//  println((a && b && !(c && d)).resolve)
//  println((!(a && b) && !(c || d)).resolve)
//  println((ha && OR(Set(AND(Set(OR(Set(a && b && (c || !(d))))))))).resolve)
//  println(((a && b) && (c || !(d || (e && (f || g))))).resolve)
//  println((c || !(d || (e && (f || g)))).resolve)
//  println((((a && b && c) || d || e || mend || mene ) && (english || welsh)).resolve)
//  println(( (a || b) && (c || d) ).resolve)
//  println(( (a || b) && c ).simplify == ((c && a) || (c && b)))
//  println((a || !(a || b)).resolve)
//  println(((a || b || c) && (d || e)).resolve)
//  println(((a || b) && (a || !(b))).resolve)
//  println((a && (b && !b)).resolve)

//  println((a && b && c) simplify DNF)
//  println((a || b || c) simplify DNF)
//  println((a || b || (c && d)) simplify CNF)
//  println(((a && b) && (c || (d || (e && (f || g))))) simplify DNF)
  println(((a && b) && (c || (d || (e && (f || g))))) simplify CNF)
}
