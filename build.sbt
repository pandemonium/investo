import Dependencies._

organization := "com.example"

scalaVersion := "2.12.4"

version := "0.1.0-SNAPSHOT"

name := "investo"

lazy val loggingDependencies = Seq(
  slf4jApi,
  log4jOverSlf4j,
  "ch.qos.logback" % "logback-core" % "1.2.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "net.logstash.logback" % "logstash-logback-encoder" % "4.9"
)

libraryDependencies ++= loggingDependencies ++ Seq(
  scalaTest % Test,
  slick,
  shapeless,
  slickPostgres,
  postgresJdbc,
  slickHikaricp,
  catsEffects
)