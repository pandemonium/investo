package investo

import domain.model._,
       domain.database._,
       core._
import cats.effect._

object Main extends App {
  import concurrent.ExecutionContext.Implicits.global

  def printUsage = IO {
    println("investo: <verb> <noun> <arguments>")
  }

  def makeProgram(commandLine: List[String]): IO[Unit] = args.toList match {
    case client.Command(command) => 
      val interpreter = Interpreter.forDbConfig("slick.db")
      interpreter.run(command)
    case _ =>
      printUsage
  }

  val program = makeProgram(args.toList)
  program.unsafeRunSync()
}