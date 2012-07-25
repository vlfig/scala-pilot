package vlfig.pilot

abstract class CurrencyZone {
  type Currency <: AbstractCurrency
  def make(n: Long): Currency

  abstract class AbstractCurrency {
    val amount: Long
    def designation: String
    override def toString = amount + " " + designation
    def +(that: Currency): Currency = make(this.amount + that.amount)
    def *(times: Int): Currency = make(this.amount * times)
  }
}

object US extends CurrencyZone {
  type Currency = Dollar
  abstract class Dollar extends AbstractCurrency {
    def designation = "USD"
  }
  def make(n: Long): Currency = {
    new Dollar { val amount = n }
  }
}

object EuroZone extends CurrencyZone {
  type Currency = Euro
  abstract class Euro extends AbstractCurrency {
    def designation = "EUR"
  }
  def make(n: Long): Currency = {
    new Euro { val amount = n }
  }
}
