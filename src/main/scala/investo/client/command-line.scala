package investo
package client

import java.time._

/**
 * - List ownership
 * investo show stocks

 * - Find all stock by pattern
 * investo search stocks Annal

 * - Info about one
 * investo show stocks nly

 * - Dividends of one
 * investo show dividends nly

 * - Forward dividends known, grouped by
 *    { year-and-company, month-and-company; year; month}
 *    (defaults to month-and-company)
 * investo show dividends --yearly --by-stock

 * - Register transaction
 * investo buy 121 nly for 10.11 USD fee 7 on 1
 * investo sell 10 alm-a for 123 SEK fee 7 on 1
 */

abstract class Extractive[A, B](pf: PartialFunction[A, B]) {
  lazy private val parser = pf.lift
  def unapply(in: A): Option[B] = parser(in)
}

object  Direction {
  sealed trait T
  case object Buy
    extends T
  case object Sell
    extends T

  object T extends Extractive[String, Direction.T]({
    case "buy"  => Buy
    case "sell" => Sell    
  })
}

object Flag {
  sealed trait T
  case object Yearly
    extends T
  case object ByStocks
    extends T

  def unapply(textual: String): Option[T] = textual match {
    case "--yearly"   => Option(Yearly)
    case "--by-stock" => Option(ByStocks)
    case _            => Option.empty
  }
}

object Command {
  sealed trait T
  case object ShowOwnedStock
    extends T
  case class SearchStock(pattern: String)
    extends T
  case class ShowStock(symbol: String)
    extends T
  case class ShowDividendReport(flags: Flag.T*) extends T {
    def implies(flag: Flag.T): Boolean =
      flags exists flag.==
  }
  case class ShowDividends(symbol: String)
    extends T
  case class TransactShares(accountId: Long,
                               symbol: String,
                            direction: Direction.T,
                                count: Int, 
                                price: Double,
                                  fee: Double,
                             currency: String, 
                                 date: LocalDate)
    extends T

  object Count {
    def unapply(text: String): Option[Int] = 
      util.Try(text.toInt).toOption
  }

  object Price {
    def unapply(text: String): Option[Double] =
      util.Try(text.toDouble).toOption
  }

  val commandParser: PartialFunction[List[String], Command.T] = {
    case "show" :: "stocks" :: Nil =>
      ShowOwnedStock
    case "search" :: "stocks" :: pattern :: Nil =>
      SearchStock(pattern)
    case "show" :: "stocks" :: symbol :: Nil =>
      ShowStock(symbol)
    case "show" :: "dividends" :: Nil =>
      ShowDividendReport()
    case "show" :: "dividends" :: Flag(arg0) :: Nil =>
      ShowDividendReport(arg0)
    case "show" :: "dividends" :: Flag(arg0) :: Flag(arg1) :: Nil =>
      ShowDividendReport(arg0, arg1)
    case "show" :: "dividends" :: symbol :: Nil =>
      ShowDividends(symbol)
    case Direction.T(tpe) :: Count(count) :: symbol :: "for" :: Price(price) :: currency :: "fee" :: Price(fee) :: "on" :: Count(accountId) :: Nil =>
      TransactShares(accountId, symbol, tpe, count, price, fee, currency, LocalDate.now)
  }

  def unapply(line: List[String]): Option[T] =
    commandParser.lift apply line
}