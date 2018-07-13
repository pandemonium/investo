package investo
package presentation

import domain.model._,
       domain.database._,
       console._, Template._
import cats.effect._
import java.time._, 
       format._,
       java.util.Locale

object Presentation {
  protected final val DateFormat = (DateTimeFormatter
    ofPattern "yyyy-MM-dd"
    withZone ZoneId.systemDefault
  )

  def showDate(i: LocalDate): String =
    DateFormat.format(i)

  def showMoney(amount: Double, locale: Locale): String = {
    val fmt = java.text.NumberFormat getCurrencyInstance locale

    fmt format amount
  }

  object StockOwnershipTable {
    type SO = Universe#Schema#TransactionsFeatures#StockOwnership

    def count(row: SO): String = 
      s"${row.count}"

    def costBasis(row: SO): String =
      showMoney(0D/*row.costBasis*/, new Locale("sv", "SE"))

    val template =
      Column(Alignment.Right, count)                ::
      Column(Alignment.Left,  (_: SO).stock.name)   ::
      Column(Alignment.Right, costBasis)            ::
      Column(Alignment.Left,  (_: SO).stock.symbol) ::
      Nil

    def render(data: Seq[SO]): String =
      template.format(data)
  }

  object DividendsTable {
    type SD = Universe#Schema#DividendsFeatures#StockDividend

    def payDate(sd: SD): String =
      showDate(sd.dividend.payDate)

    def exDate(sd: SD): String = 
      showDate(sd.dividend.exDate)

    def amount(sd: SD): String = 
      showMoney(sd.dividend.amount, sd.currency.locale)

    val template =
      Column(Alignment.Right, amount)  ::
      Column(Alignment.Left,  exDate)  ::
      Column(Alignment.Left,  payDate) ::
      Nil

    def render(data: Seq[SD]): String =
      template.format(data)    
  }

  object StocksTable {
    type S = Universe#Schema#Stock

    val template =
      Column(Alignment.Right, (_: S).symbol) ::
      Column(Alignment.Left,  (_: S).name)   ::
      Nil

    def render(data: Seq[S]): String = 
      template.format(data)
  }

  object DividendReportTable {
    type DRI = Universe#Schema#DividendsFeatures#DividendReportItem

    def payDate(item: DRI): String =
      showDate(item.dividend.payDate)

    def exDate(item: DRI): String = 
      showDate(item.dividend.exDate)

    def amount(item: DRI): String =
      showMoney(item.amountInSek, new java.util.Locale("sv", "SE"))

    def yoc(item: DRI): String =
      f"${(item.dividend.amount * item.currency.defaultRate) / 1D/*item.costBasis*/ * 100}%2.2f%%"

    def name(item: DRI): String =
      item.stock.name

    val template = 
      Column(Alignment.Left,  payDate) ::
      Column(Alignment.Right, amount)  ::
      Column(Alignment.Right, yoc)     ::
      Column(Alignment.Left,  name)    ::
      Column(Alignment.Left,  exDate)  ::
      Nil

    def render(data: Seq[DRI]): String = 
      template.format(data)
  }
}