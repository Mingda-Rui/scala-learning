class Dollars(val amount: Int) extends AnyVal {
  override def toString() = "$" + amount
}

class SwissFrancs(val amount: Int) extends AnyVal {
  override def toString() = amount + " CHF"
}

object Dollars {
  val money = new Dollars(10000000)
  val dollars = new Dollars(1000)
  val francs = new SwissFrancs(1000)
}