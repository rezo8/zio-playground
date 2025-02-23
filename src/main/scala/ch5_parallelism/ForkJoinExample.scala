package ch5_parallelism

import zio._

/*
 To Order the forked, we use Fiber.Join
 */
object ForkJoinExample extends ZIOAppDefault {
  lazy val doSomething: UIO[Unit] =
    ZIO.debug("do something!").delay(10.seconds)
  lazy val doSomethingElse: UIO[Unit] =
    ZIO.debug("do something else!").delay(2.seconds)

  override def run =
    for {
      _ <- ZIO.debug("Starting the program!")
      fiber <- doSomething.fork
      _ <- doSomethingElse
      _ <- fiber.join // Wait for the fiber to complete
      _ <- ZIO.debug("The fiber has joined!") // Only logs when doSomething finishes.
    } yield ()

  /*
    Join does not block underlying op threads.
    No thread underneath is blocked waiting for it to happen,
     instead ZIO registers a callback to be invoked when forked fiber completes.
    This allows the executor to go on to execute other fibers.
 */
}

/*
Shows how threads fail on errors for the join.
  If we comment out the join, the failure happens silently.
 */
object ForkJoinFailedFiberExample extends ZIOAppDefault {
  lazy val doSomething: ZIO[Any, String, Nothing] =
    ZIO.debug("do something!").delay(2.seconds) *> ZIO.fail("Boom!")
  override def run =
    for {
      _ <- ZIO.debug("Starting the program!")
      fiber <- doSomething.fork
      _ <- fiber.join // Fail with the error from the fiber
      _ <- ZIO.debug("The fiber has joined!")
    } yield ()
}

/*
  This shows how we can handle the returned values of a fiber.
 */
object ForkAwaitFailedFiberExample extends ZIOAppDefault {
  lazy val doSomething: ZIO[Any, String, Nothing] =
    ZIO.debug("do something!").delay(2.seconds) *> ZIO.fail("Boom!")
  override def run =
    for {
      _ <- ZIO.debug("Starting the program!")
      fiber <- doSomething.fork
      exit <- fiber.await
      _ <- exit.foldZIO( // Handle the result of the fiber
        e => ZIO.debug("The fiber has failed with: " + e),
        s => ZIO.debug("The fiber has completed with: " + s)
      )
      _ <- ZIO.debug("The fiber has joined!")
    } yield ()
}
