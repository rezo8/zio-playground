package basics

import zio.*

import java.io.IOException

object RecursionEx {

  val readInt: ZIO[Any, IOException, Int] =
    for {
      line <- Console.readLine
      int <- ZIO.attempt(line.toInt).mapError(err => IOException(err))
    } yield int

  private val readIntOrRetry: ZIO[Any, IOException, Int] = readInt.orElse(
    Console.printLine("Please enter a valid integer").zipRight(readIntOrRetry)
  )

  val run: ZIO[Any, Exception, Int] = readIntOrRetry

}
