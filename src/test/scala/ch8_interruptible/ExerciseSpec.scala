package ch8_interruptible

import ch2.ExampleSpec.{suite, test}
import zio._
import zio.test.*
import zio.test.TestAspect.*
import zio.test.Assertion.*
import zio.test.{test, *}

object ExerciseSpec extends ZIOSpecDefault {
  def spec =
    suite("ExampleSpec")(
      // Put interruptible on line
      test("interruptible") {
        for {
          ref <- Ref.make(0)
          latch <- Promise.make[Nothing, Unit]
          fiber <- ZIO
            .uninterruptible(latch.succeed(()) *> ZIO.never.interruptible)
            .ensuring(ref.update(_ + 1))
            .forkDaemon
          _ <- Live
            .live(latch.await *> fiber.interrupt.disconnect.timeout(1.second))
          value <- ref.get
        } yield assertTrue(value == 1)
      } @@ nonFlaky,
      test("uninterruptible") {
        for {
          ref <- Ref.make(0)
          latch <- Promise.make[Nothing, Unit]
          fiber <- {
            latch.succeed(()) *>
              Live.live(ZIO.sleep(10.millis)) *>
              ref.update(_ + 1)
          }.forkDaemon.uninterruptible
          _ <- latch.await *> fiber.interrupt
          value <- ref.get
        } yield assertTrue(value == 1)
      } @@ nonFlaky,
    )
}
