package ch2

import zio.*
import zio.test.*
import zio.test.Assertion.*

object ServiceSpecs extends ZIOSpecDefault {
  val goShopping: ZIO[Any, Nothing, Unit] =
    Console.printLine("Going shopping!").orDie.delay(1.hour)

  val greet: ZIO[Any, Nothing, Unit] =
    for {
      name <- Console.readLine.orDie
      _ <- Console.printLine(s"Hello, $name!").orDie
    } yield ()

  def spec =
    suite("ExampleSpec")(test("greet says hello to the user") {
      for {
        _ <- TestConsole.feedLines("Jane")
        _ <- greet
        value <- TestConsole.output
      } yield assert(value)(equalTo(Vector("Hello, Jane!\n")))
    }, test("goShopping delays for one hour") {
      for {
        fiber <- goShopping.fork
        _ <- TestClock.adjust(1.hour)
        _ <- fiber.join
      } yield assertCompletes
    })
}
