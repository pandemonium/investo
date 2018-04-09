package investo
package presentation

import domain.model._,
       domain.database._,
       console._, Template._
import cats.effect._
import java.time._, format._

object Presentation {
  protected final val DateFormat = (DateTimeFormatter
    ofPattern "yyyy-MM-dd"
    withZone ZoneId.systemDefault
  )

  protected
  def showDate(i: LocalDate): String =
    DateFormat.format(i)

  protected
  def showMoney(amount: Double,
                locale: java.util.Locale): String = {
    val fmt = java.text.NumberFormat getCurrencyInstance locale

    fmt format amount
  }

  object StockOwnershipTable {
    type SO = Universe#Schema#TransactionsFeatures#StockOwnership

    private
    def count(row: SO): String = 
      s"${row.count}"

    private
    def costBasis(row: SO): String =
      showMoney(row.costBasis, row.currency.locale)

    private
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

    private
    def payDate(sd: SD): String =
      showDate(sd.dividend.payDate)

    private
    def exDate(sd: SD): String = 
      showDate(sd.dividend.exDate)

    private
    def amount(sd: SD): String = 
      showMoney(sd.dividend.amount, sd.currency.locale)

    private
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

    private
    val template =
      Column(Alignment.Right, (_: S).symbol) ::
      Column(Alignment.Left,  (_: S).name)   ::
      Nil

    def render(data: Seq[S]): String = 
      template.format(data)
  }

  object DividendReportTable {
    type DRI = Universe#Schema#DividendsFeatures#DividendReportItem

    private
    def payDate(item: DRI): String =
      showDate(item.dividend.payDate)

    private
    def exDate(item: DRI): String = 
      showDate(item.dividend.exDate)

    private
    def amount(item: DRI): String =
      showMoney(item.amount, item.currency.locale)

    private
    def yoc(item: DRI): String =
      f"${(item.dividend.amount * item.currency.defaultRate) / item.costBasis * 100}%2.2f%%"

    private
    def name(item: DRI): String =
      item.stock.name

    private 
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