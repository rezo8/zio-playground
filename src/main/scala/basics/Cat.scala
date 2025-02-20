package basics

import zio._
object Cat extends ZIOAppDefault {
  def run =
    for {
      args <- getArgs
      allText <- ZIO.foreach(args.toList)(fileName => {
        for {
          readFile <- ReadFile.readFileZio(fileName)
          _ <- ConsoleOps.printLine(readFile)
        } yield ()
      })
    } yield ()
}
