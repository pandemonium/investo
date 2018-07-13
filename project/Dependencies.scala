import sbt._

object Dependencies {
  object Version {
    val Akka      = "2.5.8"
    val AkkaHttp  = "10.0.11"
    val Circe     = "0.8.0"
    val Slf4j     = "1.7.25"
    val Doobie    = "0.5.3"
  }

  lazy val scalaTest      = "org.scalatest"       %% "scalatest"        % "3.0.5"   withSources()
  lazy val slick          = "com.typesafe.slick"  %% "slick"            % "3.2.1"   withSources()
  lazy val shapeless      = "com.chuusai"         %% "shapeless"        % "2.3.3"   withSources()
  lazy val slickPostgres  = "com.github.tminglei" %% "slick-pg"         % "0.15.7"  withSources()
  lazy val akkaHttp       = akka("akka-http", Version.AkkaHttp)  
  lazy val postgresJdbc   = "org.postgresql"       % "postgresql"       % "42.1.4"  withSources()
  lazy val slickHikaricp  = "com.typesafe.slick"  %% "slick-hikaricp"   % "3.2.1"   withSources()
  lazy val catsEffects    = "org.typelevel"       %% "cats-effect"      % "0.10"    withSources()
  lazy val slf4jApi       = "org.slf4j"            % "slf4j-api"        % "1.7.25"  withSources()
  lazy val log4jOverSlf4j = "org.slf4j"            % "log4j-over-slf4j" % "1.7.25"  withSources()
  lazy val doobieCore     = doobie("doobie-core")
  lazy val doobiePostgres = doobie("doobie-postgres")

  private def scalaModule(name: String) =
    "org.scala-lang.modules" %% name % "1.1.0" withSources()

  private def akka(artifact: String, version: String = Version.Akka) = 
    "com.typesafe.akka" %% artifact % version withSources()

  private def doobie(artifact: String) =
    "org.tpolecat" %% artifact % Version.Doobie
}