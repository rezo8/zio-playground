package ch7_structured_concurrency

import zio._
import java.time.LocalTime
import java.time.format.DateTimeFormatter

/*
    This shows how to use scope to run a different scope within a scope.
    Importantly, forkScoped only scopes fibers to a local scope.
      Thus when the scope is closed using ZIO.scoped operator,
      the fibers forked using the default supervision or in the global scope will not be interrupted.

 */
object SimpleForkScopedExample extends ZIOAppDefault {
  private def getTime =
    LocalTime.now.format(DateTimeFormatter.ofPattern("HH:mm:ss"))

  def debug(s: String) = ZIO.debug(s"$getTime -- $s")
  def task(name: String): UIO[Unit] =
    debug(s"Fiber $name is running.")
      .schedule(Schedule.fixed(1.second))
      .onInterrupt(debug(s"Fiber $name interrupted!"))
      .ignore

  def run =
    for {
      _ <- ZIO.scoped {
        for {
          _ <- task("A").forkScoped
          _ <- debug("Fiber A forked!")
          _ <- ZIO.sleep(5.seconds)
        } yield ()
      }
      _ <- debug("Main fiber after closing the scope.")
      _ <- debug("Sleeping for 3 seconds before exiting.")
      _ <- ZIO.sleep(3.seconds)
      _ <- debug("Exiting main fiber.")
    } yield ()
}
import zio._

/*
  Note here, we are forking 2 fibers, A and E from main fiber.
    Fiber A forks 3 more fibers, with D being forked in a local scope.
      This means that D is in a different scope than B and C.
      Meaning that when A finishes after saying it will sleep for 7 seconds, D finishes as well,
      while B and C run in the main scope that is passed in through the implicit taskA.
    Since A's scope is interrupted before the 7 seconds is up (since we wait 9 seconds before closing its scope in Main) D closes prematurely.

    However! Since it forks outside of local for B and C, those continue as their scope is outside of that local scope.
    And A is still conscious, it is just it's scope that is interrupted, allowing B and C to continue until the 7 seconds in its sleep are up.

 */
object ForkScopedExample extends ZIOAppDefault {
  import SimpleForkScopedExample.debug
  import SimpleForkScopedExample.task

  lazy val taskA: ZIO[Scope, Nothing, Unit] = {
    for {
      fiberB <- task("B").fork
      _ <- debug(s"Task B forked")
      _ <- debug(s"Waiting 3 seconds after forking task B")
      _ <- ZIO.sleep(3.seconds)
      fiberC <- task("C").fork
      _ <- debug(s"Task C forked")
      _ <- debug(s"Waiting 5 seconds after forking task C")
      _ <- ZIO.sleep(5.seconds)
      fiberD <- task("D").forkScoped
      _ <- debug(s"Task D forkScoped")
      _ <- debug("Waiting 7 seconds after forkingScoped task D")
      _ <- ZIO.sleep(7.seconds)
    } yield ()
  }.onExit(e => debug(s"Fiber A exited with exit status: $e"))

  def main =
    for {
      _ <- ZIO
        .scoped {
          for {
            fiberA <- taskA.fork
            _ <- ZIO.log("Fiber A forked!")
            fiberE <- task("E").fork
            _ <- debug("Fiber E forked!")
            _ <- debug(
              "The scoped section on the main fiber is closing after 9 seconds!"
            )
            _ <- ZIO.sleep(9.seconds)
          } yield ()
        }
        .onExit(e => debug(s"Scoped section exited with exit status: $e"))
      _ <- debug(s"Main fiber after closing the scope!")
      _ <- task("F").fork.flatMap(_.interrupt.delay(11.seconds))
    } yield ()

  def run = main
}

/*
  Here we break out of the local scope and run the task is still active task in the run scope.
  This allows the scope to continue until the main scope closes, i.e. for 5 extra seconds.
 */
object ForkInExample extends ZIOAppDefault {
  def run =
    ZIO.scoped {
      for {
        outerScope <- ZIO.scope
        _ <- ZIO.scoped {
          for {
            _ <- ZIO
              .debug("Task is still active...")
              .repeat(Schedule.fixed(1.second))
              .forkIn(outerScope)
            _ <- ZIO.sleep(3.seconds)
            _ <- ZIO.debug("Closing the innermost scope.")
          } yield ()
        }
        _ <- ZIO.sleep(5.seconds)
        _ <- ZIO.debug("Closing the outermost scope.")
      } yield ()
    }
}
