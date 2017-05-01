package name.lorenzani.andrea.sudoku.sudokumodel

import org.scalatest.FlatSpec


class SValueTest extends FlatSpec {
  "SValue" should "create FinalValue when needed" in {
    val finalVal = SValue(1)
    assert(finalVal.isInstanceOf[FinalValue])
    val optionalVal = SValue(0)
    assert(optionalVal.isInstanceOf[OptionalValue])
    assertThrows[IllegalArgumentException](SValue(-1))
    assertThrows[IllegalArgumentException](SValue(10))
  }

  "OptionalValue" should "contain a list of possibility or mutate accordingly" in {
    val withOpts = OptionalValue()
    assert(withOpts.remainingValues.size == 9)
    val withOpts2 = OptionalValue(0)
    assert(withOpts2.remainingValues.size == 9)
    withOpts.remove(7)
    assert(withOpts.remainingValues.size == 8)
    assert(!withOpts.remainingValues.contains(7))
    (2 to 9).foreach(withOpts.remove) // This tests also that removing a not present number is not generating errors
    assert(withOpts.remainingValues.size == 1)
    val cloneOfWithOpts = withOpts.copy
    assert(cloneOfWithOpts.ne(withOpts))
    val mutated = withOpts.mutate
    assert(mutated.ne(withOpts))
    assert(mutated.isInstanceOf[FinalValue])
    assert(withOpts2.mutate.eq(withOpts2))
    assert(mutated.getVal == 1)
  }

  "FinalValue" should "contain one immutable value and behave accordingly" in {
    val fval = FinalValue(3)
    assertThrows[IllegalArgumentException](FinalValue(0))
    assert(fval.getVal == 3)
    fval.remove(3)
    assert(fval.getVal == 3)
    assert(fval.mutate.eq(fval))
    assert(fval.copy.ne(fval))
  }
}
