class Dollars(val amount: Int) extends AnyVal {
  override def toString() = "$" + amount
}

object Dollars {
  val money = new Dollars(10000000)
}