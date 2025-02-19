package basics

import zio.{ZIO, ZIOAppDefault}

import scala.io.StdIn

object GroceryStore extends ZIOAppDefault {

  val run: ZIO[Any, Throwable, Unit] = echoRunnable

  private def readLine = ZIO.attempt(StdIn.readLine())

  private def printLine(line: String) = ZIO.attempt(println(line))

  private def echoRunnable =
    for {
      _ <- printLine("Type your name to go to the grocery store!")
      name <- readLine
      _ <- printLine(
        s"Congratulations ${name}. You are going to the grocery store!"
      )
    } yield ()

}
