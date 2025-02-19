package basics

import zio.*

object GuessNumberGame extends ZIOAppDefault {
  def guessNumberGame(number: Int): ZIO[Any, Throwable, Unit] = {
    guessNumber(number).orElse(
      ConsoleOps
        .printLine("you guessed wrong :( try again.")
        .zipRight(guessNumberGame(number))
    )
  }

  def guessNumber(number: Int) = {
    for {
      _ <- ConsoleOps.printLine("Guess a number from 1 to 3")
      guess <- RecursionEx.readInt
      _ <- checkGuess(number, guess)
    } yield ()
  }

  def checkGuess(number: Int, guess: Int): ZIO[Any, Throwable, Unit] = {
    if (number == guess) {
      ConsoleOps.printLine("You guessed right!")
    } else {
      ZIO.fail(new Throwable(s"wrong guess of ${guess}"))
    }
  }

  def run: ZIO[Any, Throwable, Unit] =
    for {
      randomNumber <- ZIO.attempt(scala.util.Random.nextInt(3) + 1)
      _ <- guessNumberGame(randomNumber)
    } yield ()

}
