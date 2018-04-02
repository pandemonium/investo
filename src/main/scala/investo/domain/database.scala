package investo
package domain.database

import com.github.tminglei.slickpg._

trait DatabaseProfile
  extends ExPostgresProfile
     with PgDate2Support {
  def pgjson = "jsonb"

  override val api = ConcreteApi

  object ConcreteApi 
    extends API 
       with DateTimeImplicits
}

object ConcreteDatabaseProfile
  extends DatabaseProfile