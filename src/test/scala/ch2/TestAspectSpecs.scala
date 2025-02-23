package ch2

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.nonFlaky

object TestAspectSpecs extends ZIOSpecDefault {
  val goShopping: ZIO[Any, Nothing, Unit] =
    Console.printLine("Going shopping!").orDie.delay(1.hour)

  def occasionalFailure(): ZIO[Any, Throwable, String] = {
    val num = scala.util.Random.nextInt(10)
    for {
      _ <- ZIO.logInfo(s"${num}")
      _ <- {
        if (num == 1) {
          ZIO.fail(new Throwable())
        } else {
          ZIO.succeed("test")
        }
      }
    } yield { "yes" }
  }
  /*
  Use nonFlaky to test concurrent programs.
   */
  def spec =
    suite("ExampleSpec")(
      test("this test will be repeated to ensure it is stable") {
        assertZIO(ZIO.succeed(1 + 1))(equalTo(2))
      } @@ nonFlaky,
      test("this test will fail 1/10 times approximately") {
        assertZIO(occasionalFailure())(equalTo("yes"))
      } @@ nonFlaky
    ) // @@ nonFlaky can apply test aspects on a suite level.
}
