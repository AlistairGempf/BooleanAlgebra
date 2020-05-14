package booleanalgebra

import booleanalgebra.logic.{Condition, FalseCondition, TrueCondition}

object Converter {
  implicit def boolToCondition(convertBool: Boolean): Condition = {
    if (convertBool) {
      TrueCondition
    } else {
      FalseCondition
    }
  }

  implicit def intToCondition(convertInt: Int): Condition = {
    if (convertInt == 0) {
      FalseCondition
    } else {
      TrueCondition
    }
  }
}
