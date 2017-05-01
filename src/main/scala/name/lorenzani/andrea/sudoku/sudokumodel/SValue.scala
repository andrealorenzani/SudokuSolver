package name.lorenzani.andrea.sudoku.sudokumodel


abstract class SValue protected () {
  def getVal: Int = 0
  def mutate: SValue = this
  def copy: SValue
  def remove(value: Int): Unit

  override def toString: String = if(getVal != 0) getVal.toString else "*"
}

class FinalValue private (val value: Int) extends SValue {
  override def getVal: Int = value
  override def copy: SValue = new FinalValue(value)
  override def remove(value: Int): Unit = {}
}

class OptionalValue private (var remainingValues: Set[Int]) extends SValue {
  def remove(value: Int): Unit = {
    if(remainingValues.contains(value)) {
      remainingValues = remainingValues - value
    }
  }
  override def mutate: SValue = remainingValues match {
    case x if x.size==1 => SValue(x.head)
    case _ => this
  }
  override def copy: SValue = {
    val newOpt = OptionalValue()
    for(i <- 1 to 9 if !remainingValues.contains(i)) { newOpt.remove(i) }
    newOpt
  }
}

object FinalValue {
  def apply(value: Int): FinalValue = {
    require(value < 10 && value > 0)
    new FinalValue(value)
  }
}

object OptionalValue {
  def apply(): OptionalValue = new OptionalValue((1 to 9).toSet)
  def apply(x: Int): OptionalValue = {
    require(x < 10 && x >= 0)
    x match{
      case 0 => new OptionalValue((1 to 9).toSet)
      case _ => new OptionalValue(Set(x))
    }
  }
}

object SValue {
  def apply(value: Int): SValue = {
    value match {
      case 0 => OptionalValue()
      case x => FinalValue(x)
    }
  }
}