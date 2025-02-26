package ch9to13_concurrent_structures.ch11

import zio.*
import zio.http.Header.IfRange.DateTime

import java.time.{Instant, OffsetDateTime}
import java.util.concurrent.TimeUnit

object Exercises extends ZIOAppDefault {

  trait LoadBalancer[A] {
    def submit(work: A): Task[Unit]
    def shutdown: Task[Unit]
  }

  object LoadBalancer {
    def make[A](workerCount: Int, process: A => Task[A]) =
      for {
        queues <- ZIO.foreach(0 to workerCount) { _ =>
          Queue.unbounded[A]
        }
        workers <- ZIO.foreach(queues)(queue => {
          (for {
            value <- queue.take
            res <- process(value)
          } yield res).forever.fork
        })
        currentQueueIndexRef <- Ref.make(0)
        isShutdownRef <- Ref.make(false)
      } yield
        new LoadBalancer[A] {
          override def submit(work: A): Task[Unit] = {
            for {
              isShutDown <- isShutdownRef.get
              res <- if (isShutDown) {
                ZIO.fail(new IllegalStateException("LoadBalancer is shutdown"))
              } else {
                for {
                  index <- currentQueueIndexRef.getAndUpdate(oldVal => {
                    (oldVal + 1) % workerCount
                  })
                  _ <- queues(index).offer(work)
                } yield ()
              }
            } yield res
          }

          override def shutdown: Task[Unit] = isShutdownRef.update(_ => true)
        }
  }

  val loadBalancerProgram = {
    def worker: Int => Task[Int] =
      (work: Int) =>
        ZIO.logInfo(s"Processing work: $work") *>
          ZIO.sleep(1.second) *>
          ZIO.succeed(work)
    for {
      loadBalancer <- LoadBalancer.make[Int](3, worker)
      _ <- ZIO.foreachPar(1 to 10) { i =>
        loadBalancer.submit(i) // *> ZIO.logInfo(s"Submitted work: $i")
      }
      _ <- ZIO.sleep(10.second)
      _ <- loadBalancer.shutdown
      _ <- ZIO.logInfo("LoadBalancer shutdown completed")
    } yield ExitCode.success
  }

  trait RateLimiter {
    def acquire: UIO[Unit]

    def apply[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A]
  }

  object RateLimiter {
    def make(max: Int, interval: Duration): ZIO[Any, Nothing, RateLimiter] = {
      for {
        queue <- Queue.bounded[Long](max)
        _ <- ZIO.foreachDiscard(0 until max)(_ => queue.offer(0L))
      } yield
        new RateLimiter {
          override def acquire: UIO[Unit] = {
            for {
              currentTime <- Clock.currentTime(TimeUnit.MILLISECONDS)
              oldestTimestamp <- queue.take
              _ <- if (currentTime - oldestTimestamp < interval.toMillis) {
                ZIO.sleep(
                  (interval.toMillis - (currentTime - oldestTimestamp)).millis
                ) *> queue
                  .offer(currentTime)
              } else {
                queue.offer(currentTime)
              }
            } yield ()
          }

          override def apply[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
            acquire *> zio
          }
        }
    }
  }

  def doWork(taskId: Int): ZIO[Any, Nothing, Unit] = {
    for {
      _ <- ZIO.logInfo(s"Starting task $taskId")
      _ <- ZIO.sleep(500.millis) // Simulate work by sleeping
      _ <- ZIO.logInfo(s"Finished task $taskId")
    } yield ()
  }

  private val rateLimiterProgram: ZIO[Any, Nothing, Unit] = for {
    rateLimiter <- RateLimiter.make(3, 2.seconds)
    _ <- ZIO.foreachPar(1 to 10) { taskId =>
      rateLimiter(doWork(taskId))
    }
  } yield ()

  trait CircuitBreaker {
    def protect[A](operation: => Task[A]): Task[A]
  }
  object CircuitBreaker {
    def make(allowedFailures: Int) = {
      for {
        queue <- Queue.bounded(allowedFailures)
        currFailures <- Ref.make(0)
      } yield
        new CircuitBreaker {
          override def protect[A](operation: => Task[A]): Task[A] = {
            for {
              failures <- currFailures.get
              res <- if (failures >= allowedFailures) {
                ZIO.fail(new Throwable("Eclipsed failures"))
              } else {
                for {
                  proc <- queue.offer(0) // There is space to run.
                  result <- operation.onExit({
                    case Exit.Success(value) => queue.take
                    case Exit.Failure(cause) => currFailures.getAndAdd(1)
                  })
                } yield result
              }
            } yield res
          }
        }
    }
  }

  private val circuitBreakerProgram = for {
    circuitBreaker <- CircuitBreaker.make(3) // Allow 3 failures, then cooldown for 5 seconds
    _ <- ZIO.foreach(1 to 10) { i =>
      circuitBreaker
        .protect {
          for {
            _ <- ZIO.logInfo(s"Attempting operation $i")
            _ <- ZIO
              .fail(new Throwable("Operation failed"))
              .when(i % 2 == 0) // Fail every even operation
            _ <- ZIO.sleep(1.second) // Simulate work
          } yield ()
        }
        .catchAll(
          error => ZIO.logInfo(s"Operation failed: ${error.getMessage}")
        )
    }
  } yield ()

  override def run = {
    for {
      _ <- ZIO.logInfo("LOADBALANCER")
      _ <- loadBalancerProgram
      _ <- ZIO.logInfo("Rate Limiter Program")
      _ <- rateLimiterProgram
      _ <- ZIO.logInfo("Circuit Breaker")
      _ <- circuitBreakerProgram
    } yield 1
  }
}
