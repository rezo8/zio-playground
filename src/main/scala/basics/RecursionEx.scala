package basics

import zio.*
import zio.ZIOAppDefault

import java.io.IOException

object RecursionEx extends ZIOAppDefault {

  private val readInt: ZIO[Any, Throwable, Int] =
    for {
      line <- Console.readLine
      int <- ZIO.attempt(line.toInt)
    } yield int

  private val readIntOrRetry: ZIO[Any, IOException, Int] = readInt.orElse(
    Console.printLine("Please enter a valid integer").zipRight(readIntOrRetry)
  )

  val run: ZIO[Any, Exception, Int] = readIntOrRetry

}
