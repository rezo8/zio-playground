package basics

import zio.*

object AskName extends ZIOAppDefault {
  def run: ZIO[Any, Throwable, Unit] =
    for {
      _ <- ConsoleOps.printLine("What is your name")
      name <- ConsoleOps.readLine
      _ <- ConsoleOps.printLine(s"Hello $name!")
    } yield ()
}
