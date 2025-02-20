package basics

import zio.*

import java.io.IOException

object DoWhileGame extends ZIOAppDefault {
  def doWhile[R, E, A](
    body: ZIO[R, E, A]
  )(condition: A => Boolean): ZIO[R, E, A] =
    body.flatMap(x => {
      if (condition(x)) {
        for {
          _ <- ZIO.logInfo("correct!")
          res <- ZIO.succeed[A](x)
        } yield res
      } else {
        for {
          _ <- ZIO.logInfo("incorrect!")
          res <- doWhile(body)(condition)
        } yield res
      }
    })

  private def guessStringGame(string: String) = {
    for {
      _ <- ConsoleOps.printLine("do you want to play a game?")
      _ <- doWhile(Console.readLine)(guess => guess == string)
    } yield ()
  }

  private def guessNumberGame = {
    for {
      _ <- ConsoleOps.printLine("do you want to play a game?")
      number <- Random.nextIntBounded(10)
      _ <- doWhile(for {
        line <- Console.readLine
        parsedInt <- ZIO.attempt(line.toInt).mapError(err => IOException(err))
      } yield number)(guess => guess == number)
    } yield ()
  }

  def run: ZIO[Any, Throwable, Unit] =
    guessStringGame("wowza")
}
