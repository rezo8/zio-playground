package ch5_parallelism

import zio.*

/*
  This interrupts the fiber before it completes.
  If the fiber has already completed execution by the time it is interrupted,
    the returned value will be the result of the fiber.
    Otherwise, it will be a failure with Cause.Interrupt

  This is useful for safe resource usage. i.e. request gets closed midway, we add this to the processes in the call.

  Interrupting is safe in ZIO.
  Interrupts also cause any finalizers associated with that fiber to be run.

  You can configure if code is interruptible or not with ZIO combinators.
 */
object InterruptFiberExample extends ZIOAppDefault {
  lazy val doSomething: ZIO[Any, Nothing, Long] =
    ZIO
      .debug("some long running task!")
      .repeat(Schedule.spaced(2.seconds))

  override def run =
    for {
      _ <- ZIO.debug("Starting the program!")
      fiber <- doSomething.fork
      _ <- ZIO.sleep(5.seconds)
      _ <- fiber.interrupt
      _ <- ZIO.debug("The fiber has been interrupted!")
    } yield ()
}

/*
  Here is a more complex example where there are parents and child threads.

 */
import zio.Console.*

object FiberSupervisionParentChildExample extends ZIOAppDefault {
  val child: ZIO[Any, Nothing, Unit] =
    printLine("Child fiber beginning execution...").orDie *>
      ZIO.sleep(5.seconds) *>
      printLine("Hello from a child fiber!").orDie

  val parent: ZIO[Any, Nothing, Unit] =
    printLine("Parent fiber beginning execution...").orDie *>
      child.fork *>
      ZIO.sleep(3.seconds) *>
      printLine("Hello from a parent fiber!").orDie

  def run =
    for {
      fiber <- parent.fork
      _ <- ZIO.sleep(1.second)
      _ <- fiber.interrupt // since we interrupt here, there is risk that the Child fork is still running, and that's not good.
      _ <- ZIO.sleep(10.seconds)
    } yield ()

  /*
    To address this issue, ZIO has a fiber supervision model.
      The rules are as follows.
        - Every fiber has a scope.
        - Every fiber is forked in a scope.
        - Fibers are forked in the scope of the current fiber unless otherwise.
        - The scope of a fiber is closed when the fiber terminates,
              either thru success, failure or interruption.
        - When a scope is closed, all fibers forked in that scope are interrupted.

    You can also customize by adding onto the global scope using forkDaemon
    a good rule of thumb is that if you find your effects are being terminated too early,
     replace ZIO#fork with ZIO# forkDaemon.

    Another good rule of thumb is to always Fiber#join or
     Fiber#interrupt {Fiber!interrupt} any fiber you fork.
 */

}

object ForkDaemonExample extends ZIOAppDefault {
  val healthChecker: ZIO[Any, Nothing, Long] =
    ZIO
      .debug("Checking the health of the system...")
      .repeat(Schedule.spaced(1.second))
      .onInterrupt(ZIO.debug("Health checker interrupted!"))

  val parent: ZIO[Any, Nothing, Unit] = {
    for {
      _ <- ZIO.debug("Parent fiber begins execution...")
      // by forking daemon here, healthchecker will run until main app closes.
      _ <- healthChecker.forkDaemon
      _ <- ZIO.sleep(5.seconds)
      _ <- ZIO.debug("Shutting down the parent fiber!")
    } yield ()
  }.onInterrupt(ZIO.debug("Parent fiber interrupted"))

  def run =
    for {
      fiber <- parent.fork
      _ <- ZIO.sleep(1.seconds)
      _ <- fiber.interrupt
      _ <- ZIO.sleep(10.seconds)
    } yield ()
}

/*
  Locking Effects.

    Sometimes we want to execute some or all of an effect with a particular executor, rather than the default executor.
    Specifying executor can also be important if a library wants to restrict work to the thread pool
      provided by that library.
  Or if we want to run workloads on an executor specialized for those workloads.

  ZIO allows for this via their lock and on combinators.

  One important thing about onExecutor, is that it is "regional"
    Thus:
        1. When an effect is locked to an executor, all parts of that effect will be locked to that executor.
        2. Inner scopes take precedence over outer scopes.

    lazy val doSomething: UIO[Unit]     = ???
    lazy val doSomethingElse: UIO[Unit] = ???
    lazy val executor: Executor = ???
    lazy val effect = for {
      _ <- doSomething.fork
      _ <- doSomethingElse
    } yield ()
    lazy val result = effect.onExecutor(executor)


Both doSomething and doSomethingElse are guaranteed to be executed on Executor



    lazy val executor1: Executor = ???
    lazy val executor2: Executor = ???
    lazy val effect2 = for {
    _ <- doSomething.onExecutor(executor2).fork
      _ <- doSomethingElse
    } yield ()
    lazy val result2 = effect2.onExecutor(executor1)

    Now doSomething is guaranteed to be executed on executor2,
      and doSomethingElse is guaranteed to be executed on executor1.

    Together, these rules make controlling where an effect
      is executed compositional and easy to reason about.
 */
