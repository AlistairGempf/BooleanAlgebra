package booleanalgebra.logic

import org.scalatest.flatspec.AnyFlatSpec
import TestHelpers._

class FormTest extends AnyFlatSpec {
  "toDNF" should "correctly identify DNF form" in {
    assert((x).isDNF)
    assert((x && y).isDNF)
    assert((x || y).isDNF)
    assert((x || y || z).isDNF)
    assert((x || y || (w && z)).isDNF)
    assert((x || (y && z)).isDNF)
  }
  it should "correctly identify not DNF form" in {
    assert(!((x || (y && (w || z))).isDNF))
    assert(!((x && (y || z))).isDNF)
  }
  "DNF on fairly simple things" should "give an answer that matches in DNF" in {
    assertResult(x && z)((x && (y || z) && (!y || z)).normalise(DNF))
    assertResult(FalseCondition)((x && (y || z) && (!y || z) && !z).normalise(DNF))
    assertResult(x && z)((x && (y || z) && (!y || z) && z).normalise(DNF))
  }
  "DNF on fairly complex things" should "give an answer that matches in DNF" in {
    val equation = (x && (w && (x || y || (z && !w))) && (!x || z))
    val dnf = equation.normalise(DNF)
    assert(dnf isDNF)
    createTruthTable(equation).foreach(v => {
//            println(v)
      assertResult(equation(v._1, v._2))(dnf (v._1, v._2))
    })

    val equation2 = (r && s && t && (!r || v || x || (!u && y && r)) && z && (!y || u || s))
    val dnf2 = equation2.normalise(DNF)
    assert(dnf2 isDNF)
    createTruthTable(equation2).foreach(v => {
      //                  println(v)
      assertResult(equation2(v._1, v._2))(dnf2 (v._1, v._2))
    })

    val equation3 = (r || s || t || (!r && v && x && (!u || y || r)) || z || (!y && u && s))
    val dnf3 = equation3.normalise(DNF)
    assert(dnf3 isDNF)
    createTruthTable(equation3).foreach(v => {
      //                  println(v)
      assertResult(equation3(v._1, v._2))(dnf3 (v._1, v._2))
    })
  }
  "CNF on fairly simple things" should "give an answer that matches in CNF" in {
    assertResult(x && z)((x && (y || z) && (!y || z)).normalise(CNF))
    assertResult(FalseCondition)((x && (y || z) && (!y || z) && !z).normalise(CNF))
    assertResult(x && z)((x && (y || z) && (!y || z) && z).normalise(CNF))
  }
  "CNF on fairly complex things" should "give an answer that matches in CNF" in {
    val equation = (x && (w && (x || y || (z && !w))) && (!x || z))
    val dnf = equation.normalise(CNF)
    assert(dnf isCNF)
    truthTable.foreach(v => {
      //      println(v)
      assertResult(equation(v._1, v._2))(dnf (v._1, v._2))
    })

    val equation2 = (r && s && t && (!r || v || x || (!u && y && r)) && z && (!y || u || s))
    val cnf2 = equation2.normalise(CNF)
    assert(cnf2 isCNF)
    createTruthTable(equation2).foreach(v => {
      //                  println(v)
      assertResult(equation2(v._1, v._2))(cnf2 (v._1, v._2))
    })
  }
  "toCNF" should "correctly identify CNF form" in {
    assert((x).isCNF)
    assert((x || y).isCNF)
    assert((x && y).isCNF)
    assert((x && y && z).isCNF)
    assert((x && y && (w || z)).isCNF)
    assert((x && (y || z)).isCNF)
  }
  it should "correctly identify not CNF form" in {
    assert(!((x && (y || (w && z))).isCNF))
    assert(!((x || (y && z))).isCNF)
  }
}
