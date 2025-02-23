package ch5_parallelism

import zio.*
import zio.Console.printLine

object HelloWorldForker extends ZIOAppDefault {
  private val helloWorker = ZIO.sleep(2.seconds) *> ZIO.logInfo("Hello")
  private val worldWorker = ZIO.sleep(1.seconds) *> ZIO.logInfo("World")

  override def run =
    for {
      proc1 <- helloWorker.fork
      proc2 <- worldWorker.fork
      _ <- proc1.await
      _ <- proc2.await
      _ <- ZIO.logInfo("Done")
    } yield ()
}

object StopCountingNow extends ZIOAppDefault {

  private val indefiniteCounter = for {
    // Create a Ref to hold the counter value, starting at 0
    counterRef <- Ref.make(0)
    _ <- ZIO.iterate(1)(_ => true) { _ =>
      for {
        current <- counterRef.get
        _ <- ZIO.logInfo(s"$current")
        _ <- counterRef.update(_ + 1)
        _ <- ZIO.sleep(1.second)
      } yield current
    }
  } yield ()

  override def run =
    for {
      proc1 <- indefiniteCounter.fork
      _ <- ZIO.sleep(5.seconds)
      _ <- proc1.interrupt
      _ <- ZIO.logInfo("Done")
    } yield ()
}

object CanFailFiber extends ZIOAppDefault {
  private val canFail: ZIO[Any, Throwable, Unit] =
    for {
      _ <- ZIO.sleep(1.seconds)
      x <- {
        val shouldSucceed = scala.util.Random.nextBoolean()
        if (shouldSucceed) {
          ZIO.succeed(())
        } else {
          ZIO.fail(new Throwable("be true why dont you"))
        }
      }
    } yield { x }

  override def run =
    for {
      proc1 <- canFail.fork
      res <- proc1.await
      _ <- res.foldZIO(
        err => ZIO.logError(err.getMessage),
        success => ZIO.logInfo("succeeded")
      )
    } yield ()
}

object DontInterruptMe extends ZIOAppDefault {
  private val criticalOperation: ZIO[Any, Nothing, Unit] =
    ZIO.uninterruptible {
      for {
        _ <- ZIO.logInfo("Starting critical operation...")
        _ <- ZIO.sleep(3.seconds) // Simulate a long-running operation
        _ <- ZIO.logInfo("Critical operation completed!")
      } yield ()
    }

  override def run =
    for {
      fiber <- criticalOperation.fork
      _ <- ZIO.logInfo("Forked the critical operation. Waiting for 1 second...")
      _ <- ZIO.sleep(1.second)
      _ <- ZIO.logInfo("Attempting to interrupt the critical operation...")
      _ <- fiber.interrupt
      _ <- ZIO.logInfo(
        "Interrupt signal sent. Waiting for fiber to complete..."
      )
      _ <- fiber.join
      _ <- ZIO.logInfo("Done!")
    } yield ()
}

object FreeTheKid extends ZIOAppDefault {
  val child1: ZIO[Any, Nothing, Unit] =
    printLine("Child 1 fiber beginning execution...").orDie *>
      ZIO.sleep(3.seconds) *>
      printLine("Hello from child 1 fiber!").orDie

  val child2: ZIO[Any, Nothing, Unit] =
    printLine("Child 2 fiber beginning execution...").orDie *>
      ZIO.sleep(3.seconds) *>
      printLine("Hello from child 2 fiber!").orDie

  val freeChild: ZIO[Any, Nothing, Unit] =
    printLine("Free Child fiber beginning execution...").orDie *>
      ZIO.sleep(3.seconds) *>
      printLine("I am free").orDie

  val parent: ZIO[Any, Nothing, Unit] =
    printLine("Parent fiber beginning execution...").orDie *>
      child1.fork *>
      child2.fork *>
      freeChild.forkDaemon *>
      ZIO.sleep(3.seconds) *>
      printLine("Hello from a parent fiber!").orDie

  def run =
    for {
      fiber <- parent.fork
      _ <- ZIO.sleep(1.second)
      _ <- fiber.interrupt
      _ <- ZIO.sleep(10.seconds)
    } yield ()
}
