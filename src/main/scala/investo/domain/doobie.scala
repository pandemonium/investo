package investo
package domain.model

import java.time._, 
       format._,
       java.util.Locale
import doobie._,
       doobie.implicits._
import cats._, 
       cats.data._, 
       cats.implicits._

import console._, Template._

object Doobie {
  import java.time._

  case class Account(id: Long, 
                   name: String, 
     brokerageAccountId: String,
                   kind: String)

  case class Currency(id: Long,
                  symbol: String,
             defaultRate: Double,
                  locale: String)

  case class Dividend(id: Long,
                 stockId: Long,
                  amount: Double,
                  exDate: LocalDate,
                 payDate: LocalDate,
              currencyId: Long,
               projected: Boolean)

  case class Stock(id: Long,
               symbol: String,
                 name: String,
                 ISIN: String,
           currencyId: Long)

  case class Transaction(id: Long,
                    stockId: Long,
                      count: Int,
                      price: Double,
            transactionType: Int,
                 currencyId: Long,
                  pricePaid: Double,
                   bookDate: LocalDate,
                   courtage: Double,
                  accountId: Long)
}

trait AspectLike {
  import cats.effect.IO

  def transactor: Transactor[IO] = 
    Transactor.fromDriverManager[IO]("org.postgresql.Driver", 
                                     "jdbc:postgresql:investo", 
                                     "pa", 
                                     "")

  val DateFormat = (DateTimeFormatter
    ofPattern "yyyy-MM-dd"
    withZone ZoneId.systemDefault
  )

  def showDate(i: LocalDate): String =
    DateFormat.format(i)

  val MonthFmt = DateTimeFormatter.ofPattern("yyyy, MMMM")
  def showMonth(d: LocalDate): String = 
    MonthFmt.format(d)

  def showMoney(amount: Double, locale: Locale): String = {
    val fmt = java.text.NumberFormat.getCurrencyInstance(locale)

    fmt format amount
  }

  def showInt(x: Int, locale: Locale) = {
    val fmt = java.text.NumberFormat.getIntegerInstance(locale)

    fmt.format(x)
  }

  val swedish = new Locale("sv", "SE")

  implicit val localDateOrd: Ordering[LocalDate] = 
    Ordering.by(_.toEpochDay)
}

trait OwnershipAspect { self: AspectLike =>
  import Doobie._
  
  case class OwnedStock(stock: Stock, 
                       shares: Int,
                    costBasis: Double,
               costBasisTotal: Double) {
    def costBasisPerShare: Double =
      costBasis / shares
    def costBasisOfTotal: Double =
      costBasis / costBasisTotal
  }

  def percentage(quote: Double)         = f"${quote * 100}%2.1f%%"
  def shares(os: OwnedStock)            = showInt(os.shares, swedish)
  def costBasis(os: OwnedStock)         = s"${showMoney(os.costBasis, swedish)}"
  def costBasisPerShare(os: OwnedStock) = s"${showMoney(os.costBasisPerShare, swedish)}"
  def costBasisOfTotal(os: OwnedStock)  = s"${percentage(os.costBasisOfTotal)}"
  def stockName(os: OwnedStock)         = os.stock.name
  def stockSymbol(os: OwnedStock)       = os.stock.symbol

  private val template =
    Column(Alignment.Left,  stockSymbol,       "Symbol".some)     ::
    Column(Alignment.Left,  stockName,         "Stock".some)      ::
    Column(Alignment.Right, shares,            "Shares".some)     ::
    Column(Alignment.Right, costBasis,         "Cost Basis".some) ::
    Column(Alignment.Right, costBasisPerShare, "Per share".some)  ::
    Column(Alignment.Right, costBasisOfTotal,  "Size".some)       ::
    Nil

  def renderOwnedStock(data: Seq[OwnedStock]) = 
    template.format(data)

  def ownedStock(by: LocalDate): Seq[OwnedStock] = sql"""
    SELECT
      s.id, s.symbol, s.name, s."ISIN", s.currency_id,
      SUM(CASE WHEN t.transaction_type = 1 
        THEN  t.count
        ELSE -t.count
      END) AS "count",
      SUM(CASE WHEN t.transaction_type = 1 
        THEN  t.price_paid
        ELSE -t.price_paid
      END) AS "basisPrice",
      SUM(SUM(CASE WHEN t.transaction_type = 1 
        THEN  t.price_paid
        ELSE -t.price_paid
      END)) over ()
      FROM
        transactions t
      JOIN stocks s ON
        s.id = t.stock_id
      WHERE
        t.book_date < $by
      GROUP BY
        s.id, s.name
      HAVING
        SUM(CASE WHEN t.transaction_type = 1 
          THEN  t.count
          ELSE -t.count
        END) > 0
      ORDER BY 
        s.name
    """.query[OwnedStock]
       .to[Seq]
       .transact(transactor)
       .unsafeRunSync
       .filter(_.shares > 0)
}

trait DividendsAspect { self: AspectLike =>
  import Doobie._

  case class PayableDividend(stock: Stock,
                            shares: Int,
                            exDate: LocalDate,
                           payDate: LocalDate,
                    amountPerShare: Double) {
    def amount: Double = shares * amountPerShare
  }

  def dDate(pd: (LocalDate, Double))   = showMonth(pd._1)
  def dPaid(pd: (LocalDate, Double))   = showMoney(pd._2, swedish)
  def exDate(pd: PayableDividend)      = showDate(pd.exDate)
  def payDate(pd: PayableDividend)     = showDate(pd.payDate)
  def shares(pd: PayableDividend)      = showInt(pd.shares, swedish)
  def stockName(pd: PayableDividend)   = pd.stock.name
  def stockSymbol(pd: PayableDividend) = pd.stock.symbol
  def amount(pd: PayableDividend)      = showMoney(pd.amount, swedish)
  def amountPs(pd: PayableDividend)    = showMoney(pd.amountPerShare, swedish)

  private val template =
    Column(Alignment.Left,  exDate,    "Ex-Date".some)   ::
    Column(Alignment.Left,  payDate,   "Pay-Date".some)  ::
    Column(Alignment.Left,  stockName, "Stock".some)     ::
    Column(Alignment.Right, shares,    "Shares".some)         ::
    Column(Alignment.Right, amountPs,  "Per share".some) ::
    Column(Alignment.Right, amount,    "Amount".some)    ::
    Nil

  private val shortTemplate =
    Column(Alignment.Center, exDate,   "Ex-Date".some)   ::
    Column(Alignment.Center, payDate,  "Pay-Date".some)  ::
    Column(Alignment.Right,  shares,   "#".some)         ::
    Column(Alignment.Right,  amountPs, "Per share".some) ::
    Column(Alignment.Right,  amount,   "Amount".some)    ::
    Nil

  private val microTemplate = 
    Column(Alignment.Left,  dDate,   "Month".some)    ::
    Column(Alignment.Right, dPaid, "Paid".some) ::
    Nil

  def renderPayableDividends(data: Seq[PayableDividend]): String =
    template.format(data)

  def payableMonth(pd: PayableDividend) =
    pd.payDate.withDayOfMonth(1)

  def stock(pd: PayableDividend): String = 
    pd.stock.name

  def renderPayableDividendsByMonthAndStock(data: Seq[PayableDividend]): String =
    template.formatGrouped(data)(payableMonth, 
                                (d: LocalDate) => s"-- ${showMonth(d)} --")

  def renderPayableDividendsByStock(data: Seq[PayableDividend]): String =
    shortTemplate.formatGrouped(data)(stock, 
                                     (s: String) => s"-- ${s} --")

  def renderPayableDividendsByMonth(data: Seq[(LocalDate, Double)]): String =
    microTemplate.format(data)

  def dividendsByMonthAndStock(from: LocalDate, 
                            through: LocalDate): Map[LocalDate, Seq[PayableDividend]] =
    payableDividends(from, through) groupBy payableMonth

  def dividendsByMonth(from: LocalDate, 
                    through: LocalDate): Seq[(LocalDate, Double)] =
    dividendsByMonthAndStock(from, through)
      .mapValues(_.map(_.amount).sum)
      .toSeq
      .sortBy(_._1)

  def payableDividends(from: LocalDate, 
                    through: LocalDate): Seq[PayableDividend] = sql"""
    SELECT
      d.stock_id, s.symbol, s.name, s."ISIN", s.currency_id,
      SUM(CASE WHEN transaction_type = 1 
        THEN  count
        ELSE -count
      END) AS "Count",
      d.ex_date, d.pay_date, d.amount * c.default_rate
      FROM transactions t
      JOIN dividends d ON
        d.stock_id = t.stock_id AND 
        t.book_date < d.ex_date
      JOIN stocks s ON
        s.id = d.stock_id
      JOIN currencies c ON
        c.id = d.currency_id
      GROUP BY
        d.id, s.name, s.symbol, s."ISIN", s.currency_id, c.default_rate
      HAVING 
        d.pay_date > $from AND
        d.pay_date <= $through
      ORDER BY
        d.pay_date
  """.query[PayableDividend]
     .to[Seq]
     .transact(transactor)
     .unsafeRunSync
     .filter(_.shares > 0)

  def accumulatedDividends(by: LocalDate): Map[Stock, Double] = 
    Map.empty
}

trait TransactionsAspect { self: AspectLike =>
  import Doobie._

  case class Exchange(count: Int, 
                      price: Double)

  case class StockChange(year: Int,
                        month: Int,
                        stock: Stock,
                         buys: Exchange,
                        sells: Exchange,
                         fees: Double) {
    import scala.util.Try

    def boughtAverage: Option[Double] = 
      if (buys.count > 0)
        Option(buys.price / buys.count)
      else Option.empty

    def soldAverage: Option[Double] = 
      if (sells.count > 0)
        Option(sells.price / sells.count)
      else Option.empty

    def netExchange = 
      Exchange(buys.count - sells.count,
               sells.price - buys.price - fees)
    def date = 
      LocalDate.of(year, month, 1)
  }

  def date(sc: StockChange)       = showMonth(sc.date)
  def stockName(sc: StockChange)  = sc.stock.name

  def bought(sc: StockChange)     = showInt(sc.buys.count, swedish)
  def boughtPaid(sc: StockChange) = showMoney(sc.buys.price, swedish)
  def boughtAvg(sc: StockChange)  = 
    sc.boughtAverage map (showMoney(_, swedish)) getOrElse ""

  def sold(sc: StockChange)       = showInt(sc.sells.count, swedish)
  def soldRec(sc: StockChange)    = showMoney(sc.sells.price, swedish)
  def soldAvg(sc: StockChange)    = 
    sc.soldAverage map (showMoney(_, swedish)) getOrElse ""

  def change(sc: StockChange)     = showInt(sc.netExchange.count, swedish)
  def net(sc: StockChange)        = showMoney(sc.netExchange.price, swedish)

  def fees(sc: StockChange)       = showMoney(sc.fees, swedish)

  private val template =
    Column(Alignment.Left,  date,       "Month".some)    ::
    Column(Alignment.Left,  stockName,  "Stock".some)    ::
    Column(Alignment.Right, bought,     "Bought".some)   ::
    Column(Alignment.Right, boughtPaid, "Paid".some)     ::
    Column(Alignment.Right, boughtAvg,  "Average".some)  ::
    Column(Alignment.Right, sold,       "Sold".some)     ::
    Column(Alignment.Right, soldRec,    "Received".some) ::
    Column(Alignment.Right, soldAvg,    "Average".some)  ::
    Column(Alignment.Right, change,     "Change".some)   ::
    Column(Alignment.Right, net,        "Net".some)      ::
    Column(Alignment.Right, fees,       "Fees".some)     ::
    Nil

  def renderStockChanges(changes: Seq[StockChange]): String =
    template.format(changes)

  def renderStockChanges2(changes: Seq[StockChange]): String =
    template.tail.formatGrouped(changes)(_.date, (d: LocalDate) => s"-- ${showMonth(d)} --")

  def stockChangesByMonth(from: LocalDate, 
                       through: LocalDate): Seq[StockChange] = sql"""
    SELECT
      date_part('year', t.book_date)  AS year, 
      date_part('month', t.book_date) AS month,
      s.id, s.symbol, s.name, s."ISIN", s.currency_id,
      SUM(CASE WHEN transaction_type = 1 
        THEN count
        ELSE 0
      END) AS bought_count,
      SUM(CASE WHEN transaction_type = 1
        THEN count * t.price * c.default_rate
        ELSE 0
      END) AS bought_price,
      SUM(CASE WHEN transaction_type = 0 
        THEN count
        ELSE 0
      END) AS sold_count,
      SUM(CASE WHEN transaction_type = 0
        THEN count * t.price * c.default_rate
        ELSE 0
      END) AS sold_price,
      SUM(t.courtage) AS fees
      FROM transactions t
      JOIN stocks s ON
        s.id = t.stock_id
      JOIN currencies c ON
        c.id = s.currency_id
      WHERE
        t.book_date > $from AND
        t.book_date <= $through
      GROUP BY
        year, month, s.id
      ORDER BY 
        year, month, s.name
  """.query[StockChange]
     .to[Seq]
     .transact(transactor)
     .unsafeRunSync
}

trait PortfolioAspect { self: AspectLike with TransactionsAspect =>
  import Doobie._

  case class Summary(date: LocalDate,
                   bought: Double,
                     sold: Double,
                      net: Double,
                     fees: Double)

  def sDate(summary: Summary)   = showMonth(summary.date)
  def sBought(summary: Summary) = showMoney(summary.bought, swedish)
  def sSold(summary: Summary)   = showMoney(summary.sold, swedish)
  def sNet(summary: Summary)    = showMoney(summary.net, swedish)
  def sFees(summary: Summary)   = showMoney(summary.fees, swedish)

  private val template =
    Column(Alignment.Left,  sDate,   "Month".some)    ::
    Column(Alignment.Right, sBought, "Bought".some) ::
    Column(Alignment.Right, sSold,   "Sold".some) ::
    Column(Alignment.Right, sNet,    "Net".some)      ::
    Column(Alignment.Right, sFees,   "Fees".some)     ::
    Nil

  def renderPortfolioChanges(summaries: Seq[Summary]): String =
    template.format(summaries)

  def portfolioChangesByMonth(from: LocalDate,
                           through: LocalDate): Seq[Summary] =
    stockChangesByMonth(from, through)
      .groupBy(_.date)
      .toSeq
      .map { case (date, values) =>
        val bought = values.map(_.buys.price).sum
        val sold   = values.map(_.sells.price).sum
        val net    = bought - sold
        val fees   = values.map(_.fees).sum

        Summary(date, bought, sold, net, fees)
      }
      .sortBy(_.date)
}

object InvestoUniverse extends AnyRef
  with AspectLike
  with OwnershipAspect
  with DividendsAspect
  with TransactionsAspect
  with PortfolioAspect

object Main extends App {
  val ownership = InvestoUniverse.ownedStock(LocalDate.now)

  val dividends = InvestoUniverse.payableDividends(LocalDate.parse("2018-01-01"), 
                                                   LocalDate.parse("2019-01-31"))

  val changes = InvestoUniverse.stockChangesByMonth(LocalDate.parse("2018-01-01"), 
                                                    LocalDate.parse("2018-12-31"))

  val portfolioChanges =
    InvestoUniverse.portfolioChangesByMonth(LocalDate.parse("2018-01-01"),
                                            LocalDate.parse("2018-12-31"))

  val monthDivs = InvestoUniverse.dividendsByMonth(LocalDate.parse("2018-01-01"),
                                                   LocalDate.parse("2018-12-31"))

//  println(InvestoUniverse.renderOwnedStock(ownership))
//  println(InvestoUniverse.renderPortfolioChanges(portfolioChanges))
  println(InvestoUniverse.renderPayableDividendsByMonthAndStock(dividends))
//  println(InvestoUniverse.renderOwnedStock(ownership))
}