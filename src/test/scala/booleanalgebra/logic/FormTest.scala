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
    assertResult(x || z)((x || (y && z) || (!y && z) || z).normalise(DNF))
    assertResult(x || (y && z))((x || (y && z)).normalise(DNF))
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

    val equation4 = (x && y && (z || r || !(y && t && (u || !v)) || w) && u)
    val dnf4 = equation4.normalise(DNF)
    assert(dnf4 isDNF)
    createTruthTable(equation4).foreach(v => {
//      println(v)
      assertResult(equation4(v._1, v._2))(dnf4 (v._1, v._2))
    })

    val equation5 = !(y && t && (u || !v))
    val dnf5 = equation5.normalise(DNF)
    assert(dnf5 isDNF)
    createTruthTable(equation5).foreach(v => {
      //      println(v)
      assertResult(equation5(v._1, v._2))(dnf5 (v._1, v._2))
    })

    val equation6 = a & b & (c | d | (e & f & !(h & i & (!j | k)) & !c & d)) & !k
    val dnf6 = equation6.normalise(DNF)
    assert(dnf6 isDNF)
    createTruthTable(equation6).foreach(v => {
      //      println(v)
      assertResult(equation6(v._1, v._2))(dnf6 (v._1, v._2))
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

    val equation3 = (r || s || t || (!r && v && x && (!u || y || r)) || z || (!y && u && s))
    val cnf3 = equation3.normalise(CNF)
    assert(cnf3 isCNF)
    createTruthTable(equation3).foreach(v => {
      //                  println(v)
      assertResult(equation3(v._1, v._2))(cnf3 (v._1, v._2))
    })

    val equation4 = (x && y && (z || r || !(y && t && (u || !v)) || w) && u)
    val cnf4 = equation4.normalise(CNF)
    assert(cnf4 isCNF)
    createTruthTable(equation4).foreach(v => {
      //      println(v)
      assertResult(equation4(v._1, v._2))(cnf4 (v._1, v._2))
    })

    val equation5 = !(y && t && (u || !v))
    val cnf5 = equation5.normalise(CNF)
    assert(cnf5 isCNF)
    createTruthTable(equation5).foreach(v => {
      //      println(v)
      assertResult(equation5(v._1, v._2))(cnf5 (v._1, v._2))
    })

    val equation6 = a & b & (c | d | (e & f & !(h & i & (!j | k)) & !c & d)) & !k
    val cnf6 = equation6.normalise(CNF)
    assert(cnf6 isCNF)
    createTruthTable(equation6).foreach(v => {
      //      println(v)
      assertResult(equation6(v._1, v._2))(cnf6 (v._1, v._2))
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
