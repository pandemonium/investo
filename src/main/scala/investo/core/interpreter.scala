package investo
package core

import domain.model._,
       domain.database._,
       console._, Template._
import cats.effect._
import cats._, 
       cats.data._, 
       cats.implicits._
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
         Interpreter._,
         presentation._
  
  def putStrLn(str: String): IO[Unit] =
    IO(println(str))

  def now: IO[LocalDate] =
    IO(LocalDate.now)

  def run(    command: Command.T)
         (implicit ec: ExecutionContext): IO[Unit] = {

    def lift[A](action: => DBIO[A])(implicit db: Database): IO[A] =
      IO.fromFuture(IO(db.run(action)))

    def interpretCommand(implicit db: Database) = command match {
      case Command.ShowOwnedStock =>
        (lift(schema.transactions ownedStockBy LocalDate.now)
          map Presentation.StockOwnershipTable.render
          >>= putStrLn)

      case Command.SearchStock(pattern) =>
        (lift(schema.stocks byPattern pattern)
          map Presentation.StocksTable.render
          >>= putStrLn)

      case Command.ShowDividends(symbol) =>
        (lift(schema.dividends byStockSymbol symbol)
          map Presentation.DividendsTable.render
          >>= putStrLn)

      case Command.ShowDividendReport(flags @ _*) =>
        (lift(schema.dividends receivableAsOf LocalDate.now)
          map Presentation.DividendReportTable.render
          >>= putStrLn)

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
          _        <- putStrLn("Ok")
        } yield ()

      case x =>
        putStrLn(s"Not implemented: $x")
    }

    for {
      db <- IO(Database.forConfig(dbConfig))
       x <- interpretCommand(db)
      _  <- IO.fromFuture(IO(db.shutdown))
    } yield x
  }
}