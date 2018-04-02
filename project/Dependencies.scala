import sbt._

object Dependencies {
  object Version {
    val Akka      = "2.5.8"
    val AkkaHttp  = "10.0.11"
    val Circe     = "0.8.0"
    val Slf4j     = "1.7.25"
  }

  lazy val scalaTest      = "org.scalatest"       %% "scalatest"        % "3.0.5"
  lazy val slick          = "com.typesafe.slick"  %% "slick"            % "3.2.1"
  lazy val shapeless      = "com.chuusai"         %% "shapeless"        % "2.3.3"
  lazy val slickPostgres  = "com.github.tminglei" %% "slick-pg"         % "0.15.7"
  lazy val akkaHttp       = akka("akka-http", Version.AkkaHttp)  
  lazy val postgresJdbc   = "org.postgresql"       % "postgresql"       % "42.1.4"
  lazy val slickHikaricp  = "com.typesafe.slick"  %% "slick-hikaricp"   % "3.2.1"
  lazy val catsEffects    = "org.typelevel"       %% "cats-effect"      % "0.10"
  lazy val slf4jApi       = "org.slf4j"            % "slf4j-api"        % "1.7.25"
  lazy val log4jOverSlf4j = "org.slf4j"            % "log4j-over-slf4j" % "1.7.25"

  private def scalaModule(name: String) =
    "org.scala-lang.modules" %% name % "1.1.0"

  private def akka(artifact: String, version: String = Version.Akka) = 
    "com.typesafe.akka" %% artifact % version withSources()
}