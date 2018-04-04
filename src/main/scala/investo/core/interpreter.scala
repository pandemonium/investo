package investo
package core

import domain.model._,
       domain.database._,
       console._, Template._
import cats.effect._
import java.time._, format._

object Interpreter {
  implicit class OptionOps[A](val o: Option[A]) extends AnyVal {
    def zip[B](p: Option[B]): Option[(A, B)] = for {
      a <- o
      b <- p
    } yield (a, b)

    def bimap[B, C](f: B => C)(implicit ev: A <:< (B, B)): Option[(C, C)] =
      o map { a =>
        ev(a) match {
          case (b1, b2) => (f(b1), f(b2))
        }
      }

    def biflatMap[B, C](f: B => Option[C])(implicit ev: A <:< (B, B)): Option[(C, C)] =
      o flatMap { a =>
        ev(a) match {
          case (b1, b2) => f(b1) zip f(b2)
        }
      }
  }

  def forDbConfig(name: String): Interpreter =
    Interpreter(Universe(ConcreteDatabaseProfile), name)
}


case class Interpreter(universe: Universe,
                       dbConfig: String) {
  import client._,
         universe._,
         profile.api._,
         concurrent._,
         Interpreter._

  protected final val DateFormat = (DateTimeFormatter
    ofPattern "yyyy-MM-dd"
    withZone ZoneId.systemDefault
  )

  protected
  def showDate(i: LocalDate): String =
    DateFormat.format(i)

  protected
  def showMoney(amount: Double,
              currency: schema.Currency): String = {
    import java.text._,
           java.util._

    val fmt = NumberFormat getCurrencyInstance currency.locale
    fmt setCurrency Currency.getInstance(currency.symbol)

    fmt format amount
  }

  def run(command:     Command.T)
         (implicit ec: ExecutionContext): IO[Unit] = {

    def lift[A](action: DBIO[A])(implicit db: Database): IO[A] =
      IO.fromFuture(IO(db.run(action)))

    def interpretCommand(implicit db: Database) = command match {
      case Command.ShowOwnedStock =>
        for {
          result <- lift(schema.transactions ownedStockBy LocalDate.now)
          _      <- IO {
            import schema.transactions.{ StockOwnership => SO }
            val cols = Column(Alignment.Right, (_: SO).count.getOrElse(0).toString) ::
                       Column(Alignment.Left, (_: SO).stock.name) ::
                       Column(Alignment.Right, (_: SO).costBasis.getOrElse(0D).toString) ::
                       Nil

              println(cols.format(result))
            
/*            result foreach {
              case schema.transactions.StockOwnership(stock, currency, Some(count), Some(basis)) =>
                println(s"$count\t(${showMoney(basis, currency)})\t${stock.name}")
              case _ =>
}*/
          }
        } yield ()

      case Command.SearchStock(pattern) =>
        for {
          result <- lift(schema.stocks byPattern pattern)
          _      <- IO {
            result foreach {
              case schema.Stock(_, _, symbol, name, _) =>
                println(s"($symbol) $name")
            }
          }
        } yield ()

      case Command.ShowDividends(symbol) =>
        for {
          result <- lift(schema.dividends byStockSymbol symbol)
          _      <- IO {
            result foreach {
              case schema.dividends.StockDividend(_, div, currency) =>
                println(s"${showMoney(div.amount, currency)} (${showDate(div.exDate)}) ${showDate(div.payDate)}")
            }
          }
        } yield ()

      case Command.ShowDividendReport(flags @ _*) =>
        for {
          result <- lift(schema.dividends receivableAsOf LocalDate.now)
          _      <- IO {
            result foreach {
              case item @ schema.dividends.DividendReportItem(s, d, c, shares) =>
                println(s"${showDate(d.payDate)} ${showMoney(item.amount, c)}\t${s.name} (${showDate(d.exDate)})")
            }
          }
        } yield ()

      case Command.TransactShares(accountId, sym, direction, cnt, price, fee, curr, date) =>
        val pricePaid       = cnt * price + fee
        val transactionType = direction match {
          case client.Direction.Buy  => schema.TransactionType.Buy 
          case client.Direction.Sell => schema.TransactionType.Sell
        }

        def mkTransaction(stockId: schema.stocks.Id,
                       currencyId: schema.currencies.Id): schema.Transaction =
          schema.Transaction(schema.transactions.New,
                             stockId,
                             cnt,
                             price,
                             transactionType,
                             currencyId,
                             pricePaid,
                             date,
                             fee,
                             schema.accounts mkId accountId)

        def mkInsertable: DBIO[schema.Transaction] = for {
          stockId    <- schema.stocks idFromSymbol sym
          currencyId <- schema.currencies idFromSymbol curr
        } yield mkTransaction(stockId, currencyId)

        for {
          tx       <- lift(mkInsertable)
          result   <- lift(schema.transactions += tx)
          _        <- IO { println("Ok") }
        } yield ()

      case x =>
        IO {
          println(s"Not implemented: $x")
        }
    }

    for {
      db <- IO(Database.forConfig(dbConfig))
       x <- interpretCommand(db)
      _  <- IO.fromFuture(IO(db.shutdown))
    } yield x
  }
}