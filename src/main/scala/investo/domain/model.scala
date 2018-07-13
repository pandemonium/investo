package investo
package domain.model

import scala.language.implicitConversions

import java.time._
import slick.jdbc.JdbcProfile

import domain.database._

case class Universe(profile: DatabaseProfile) {
  import profile.api._

  trait AspectLike { self: SchemaLike => 
    def datePart(partName: String) =
      (c: Rep[LocalDate]) =>
        SimpleFunction[Int]("date_part").apply(Seq(partName, c))

    def year(c: Rep[LocalDate]) =
      SimpleFunction[Int]("date_part").apply(Seq(LiteralColumn("year"), c))

    def month(c: Rep[LocalDate]) =
      SimpleFunction[Int]("date_part").apply(Seq(LiteralColumn("month"), c))
  }

  trait SchemaLike {
    trait Domain {
      import shapeless._, 
             tag._

      type Tag
      type Id         = Long @@ Tag
      type PrimaryKey = Option[Id]

      def mkId(value: Long): Id = tag[Tag][Long](value)
      def New: PrimaryKey       = Option.empty

      implicit def Implicit =
        MappedColumnType.base[Id, Long](identity, mkId)
    }
  }

  trait AccountsAspect extends AspectLike { self: SchemaLike =>
    import accounts.Implicit

    case class Account(id: accounts.PrimaryKey,
                     name: String,
       brokerageAccountId: String,
                     kind: String)

    case class AccountsTable(tag: Tag) extends Table[Account](tag, "accounts") {
      def id                 = column[accounts.Id]("id", O.PrimaryKey, O.AutoInc)
      def name               = column[String]     ("name")
      def brokerageAccountId = column[String]     ("brokerage_account_id")
      def kind               = column[String]     ("kind")

      def * = (id.?, name, brokerageAccountId, kind) <> 
        (Account.tupled, Account.unapply)
    }

    object accounts
      extends TableQuery(AccountsTable)
         with Domain
  }

  trait StocksAspect extends AspectLike { self: SchemaLike with CurrenciesAspect =>
    import stocks.Implicit
    import currencies.{Implicit => CurrencyImplicit}

    case class Stock(id: stocks.PrimaryKey,
             currencyId: currencies.Id,
                 symbol: String,
                   name: String,
                   isin: String)

    case class CostBasis(local: Double,
                         swedish: Double)

    case class StocksTable(tag: Tag) extends Table[Stock](tag, "stocks") {
      def id         = column[stocks.Id]    ("id", O.PrimaryKey, O.AutoInc)
      def currencyId = column[currencies.Id]("currency_id")
      def symbol     = column[String]       ("symbol")
      def name       = column[String]       ("name")
      def isin       = column[String]       ("ISIN")

      def * = (id.?, currencyId, symbol, name, isin) <>
        (Stock.tupled, Stock.unapply)
    }

    trait StocksFeatures { self: TableQuery[StocksTable] =>
      def bySymbol(symbol: String): DBIO[Stock] = 
        filter(_.symbol === symbol).result.head

      def idFromSymbol(symbol: String): DBIO[stocks.Id] =
        filter(_.symbol === symbol).map(_.id).result.head

      def byPattern(pattern: String): DBIO[Seq[Stock]] = filter { s =>
        (s.symbol like s"%$pattern%") || 
        (s.name   like s"%$pattern%")
      }.result
    }

    object stocks
      extends TableQuery(StocksTable)
         with StocksFeatures
         with Domain
  }

  trait DividendsAspects { self: SchemaLike with StocksAspect 
                                            with CurrenciesAspect
                                            with TransactionsAspect =>
    import dividends.Implicit
    import stocks.{Implicit => StockImplicit}
    import currencies.{Implicit => CurrencyImplicit}
    import transactions.{Implicit => TransactionsImplicit}

    case class Dividend(id: dividends.PrimaryKey,
                   stockId: stocks.Id,
                    amount: Double,
                    exDate: LocalDate,
                   payDate: LocalDate,
                currencyId: currencies.Id)

    case class DividendsTable(tag: Tag) extends Table[Dividend](tag, "dividends") {
      def id         = column[dividends.Id] ("id", O.PrimaryKey, O.AutoInc)
      def stockId    = column[stocks.Id]    ("stock_id")
      def amount     = column[Double]       ("amount")
      def exDate     = column[LocalDate]    ("ex_date")
      def payDate    = column[LocalDate]    ("pay_date")
      def currencyId = column[currencies.Id]("currency_id")

      def * = (id.?, stockId, amount, exDate, payDate, currencyId) <>
        (Dividend.tupled, Dividend.unapply)
    }

    trait DividendsFeatures { self: TableQuery[DividendsTable] =>
      case class StockDividend(stock: Stock,
                            dividend: Dividend,
                            currency: Currency)

      case class DividendReportItem(stock: Stock,
                                 dividend: Dividend,
                                 currency: Currency,
                                costBasis: (Double, Double),
                                   shares: Int) {
        def amountInSek: Double =
          dividend.amount * shares * currency.defaultRate
      }

      def byStockSymbol(symbol: String): DBIO[Seq[StockDividend]] = (dividends
        join stocks on (_.stockId === _.id)
        filter { case (_, s) =>
          s.symbol === symbol
        } join currencies on { case ((d, _), c) =>
          d.currencyId === c.id 
        } map { case ((d, s), c) => 
          (s, d, c).mapTo[StockDividend]
        }
      ).result

      def receivableAsOf(deadline: LocalDate): DBIO[Seq[DividendReportItem]] = 
        (transactions
          join dividends on { case (t, d) =>
            (d.stockId === t.stockId) &&
            (t.bookDate < d.exDate)
          } join stocks on { case ((t, d), s) =>
            s.id === d.stockId
          } join currencies on { case (((t, d), s), c) =>
            c.id === d.currencyId
          } filter { case (((t, d), s), c) =>
            d.exDate > deadline
          } groupBy { case (((t, d), s), c) =>
            (d, s, c)
          } map { case ((d, s, c), group) =>
            val ts        = group map { case (((t, _), _), _) => t }
            val shares    = ts map sellCountsAsNegative
            val costBasis = transactions.costBasisQuery(ts)

            (s, d, c, (0D, 0D), shares.sum getOrElse 0)
          } sortBy { case (_, dividend, _, _, _) =>
            dividend.payDate.asc
          } map { case item =>
            item.mapTo[DividendReportItem]
          }
        ).result
    }

    object dividends
      extends TableQuery(DividendsTable)
         with DividendsFeatures
         with Domain
  }

  trait CurrenciesAspect { self: SchemaLike =>
    import currencies.Implicit
    import java.util.Locale

    implicit def localeColumn = MappedColumnType.base[Locale, String](
      _.getDisplayName,
      x => (x split '_').toList match {
        case lang :: country :: _ =>
          new Locale(lang, country)
        case _ =>
          new Locale(x)
      }
    )

    case class Currency(id: currencies.PrimaryKey,
                    symbol: String,
               defaultRate: Double,
                    locale: Locale)

    case class CurrenciesTable(tag: Tag) extends Table[Currency](tag, "currencies") {
      def id          = column[currencies.Id]("id", O.PrimaryKey, O.AutoInc)
      def symbol      = column[String]       ("symbol")
      def defaultRate = column[Double]       ("default_rate")
      def locale      = column[Locale]       ("locale")

      def * = (id.?, symbol, defaultRate, locale) <> 
        (Currency.tupled, Currency.unapply)
    }

    trait CurrenciesFeatures { self: TableQuery[CurrenciesTable] =>
      def bySymbol(symbol: String): DBIO[Currency] = 
        filter(_.symbol === symbol).distinct.result.head

      def idFromSymbol(symbol: String): DBIO[currencies.Id] =
        filter(_.symbol === symbol).map(_.id).result.head
    }

    object currencies
      extends TableQuery(CurrenciesTable)
         with CurrenciesFeatures
         with Domain
  }

  trait TransactionsAspect { self: SchemaLike with StocksAspect 
                                              with CurrenciesAspect 
                                              with AccountsAspect =>
    import transactions.Implicit
    import stocks.{    Implicit => StockImplicit}
    import currencies.{Implicit => CurrencyImplicit}
    import accounts.{  Implicit => AccountImplicit}

    object TransactionType {
      sealed trait T
      case object Buy
        extends T
      case object Sell
        extends T
      case class Unknown(x: Int)
        extends T

      val buy: T  = Buy
      val sell: T = Sell

      def fromInt(x: Int): T = x match {
        case 0 => Sell
        case 1 => Buy
        case _ => Unknown(x)
      }

      def toInt(`type`: T): Int = `type` match {
        case Sell       => 0
        case Buy        => 1
        case Unknown(x) => x
      }

      implicit def transactionType =
        MappedColumnType.base[T, Int](toInt, fromInt)
    }

    case class Transaction(id: transactions.PrimaryKey,
                      stockId: stocks.Id,
                        count: Int,
                        price: Double,
              transactionType: TransactionType.T,
                   currencyId: currencies.Id,
                    pricePaid: Double,
                     bookDate: LocalDate,
                     courtage: Double,
                    accountId: accounts.Id)

    case class TransactionsTable(tag: Tag) extends Table[Transaction](tag, "transactions") {
      import TransactionType._

      def id              = column[transactions.Id]  ("id", O.PrimaryKey, O.AutoInc)
      def stockId         = column[stocks.Id]        ("stock_id")
      def count           = column[Int]              ("count")
      def price           = column[Double]           ("price")
      def transactionType = column[TransactionType.T]("transaction_type")
      def currencyId      = column[currencies.Id]    ("currency_id")
      def pricePaid       = column[Double]           ("price_paid")
      def bookDate        = column[LocalDate]        ("book_date")
      def courtage        = column[Double]           ("courtage")
      def accountId       = column[accounts.Id]      ("account_id")

      def * = (id.?, stockId, count, price, transactionType, currencyId, pricePaid, bookDate, courtage, accountId) <>
        (Transaction.tupled, Transaction.unapply)
    }

    def sellCountsAsNegative(tx: TransactionsTable): Rep[Int] = (Case 
      If tx.transactionType === TransactionType.buy
        Then tx.count
        Else tx.count * -1)

    def sellCountsAsZero(tx: TransactionsTable): Rep[Int] = (Case 
      If tx.transactionType === TransactionType.buy
        Then tx.count
        Else 0)

    def pricePaidInSek(tx: TransactionsTable): Rep[Double] = (Case 
      If tx.transactionType === TransactionType.buy
        Then tx.pricePaid
        Else tx.courtage)

    def swedishByLocalRate(tx: TransactionsTable): Rep[Double] =
      (tx.pricePaid - tx.courtage.asColumnOf[Double]) / (tx.price * tx.count.asColumnOf[Double])

    trait TransactionsFeatures { self: TableQuery[TransactionsTable] =>
      case class StockOwnership(stock: Stock,
                             currency: Currency,
                                count: Int,
                            costBasis: Option[(Double, Double)])

      def costBasisQuery(ts: Query[TransactionsTable, Transaction, Seq]): Rep[Option[(Double, Double)]] = {
        val boughtCount      = ts.map(sellCountsAsZero).sum map { c =>
          Case If c === 0 Then 1 Else c
        }
        val boughtSekValue   = ts map pricePaidInSek
        val boughtLocalValue = ts map (t => pricePaidInSek(t) / swedishByLocalRate(t))

        for {
          local <- boughtLocalValue.sum
          sek   <- boughtSekValue.sum
          count <- boughtCount
        } yield (local / count.asColumnOf[Double], sek / count.asColumnOf[Double])

        ???
      }

      def ownedStockBy(deadline: LocalDate): DBIO[Seq[StockOwnership]] = (transactions
        filter(_.bookDate <= deadline)
        join stocks on { case (t, s) => 
          t.stockId === s.id
        } join currencies on { case ((t, s), c) => 
          t.currencyId === c.id
        } groupBy { case ((t, s), c) => 
          (s, c) 
        } map { case ((s, c), group) =>
            val ts         = group map { case ((t, _), _) => t }
            val ownedCount = ts map sellCountsAsNegative
            val costBasis  = costBasisQuery(ts)

            (s, c, ownedCount.sum getOrElse 0, costBasis).mapTo[StockOwnership]
        }
      ).result
    }

    object transactions
      extends TableQuery(TransactionsTable)
         with TransactionsFeatures
         with Domain
  }

  abstract class Schema
    extends SchemaLike
       with AccountsAspect
       with DividendsAspects
       with StocksAspect
       with CurrenciesAspect
       with TransactionsAspect

  object schema extends Schema  
}