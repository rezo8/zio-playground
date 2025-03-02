package ch14to16_resource_management.ch14_acquire_release

import zio.*

import java.net.Socket
import scala.util.{Failure, Success, Try}

object Example extends ZIOAppDefault {

  object LegacySendData {
    def sendData(host: String, port: Int, data: Array[Byte]): Try[Int] = {
      var socket: Socket = null
      try {
        socket = new Socket(host, port)
        val out = socket.getOutputStream
        out.write(data)
        Success(data.length)
      } catch {
        case e: Exception => Failure(e)
      } finally {
        if (socket != null) socket.close()
      }
    }
  }

  // rewrite the function using ZIO
  def sendData(host: String, port: Int, data: Array[Byte]): Task[Int] =
    ZIO.acquireReleaseWith(ZIO.attempt(new Socket(host, port)))(
      x => ZIO.succeed(x.close())
    ) { socket =>
      for {
        out <- ZIO.attempt(socket.getOutputStream)
        _ <- ZIO.attempt(out.write(data))
      } yield data.length
    }

  object ZIOHomeMade {
    def acquireReleaseWith[R, E, A, B](acquire: ZIO[R, E, A])(
      release: A => ZIO[R, Nothing, Any]
    )(use: A => ZIO[R, E, B]): ZIO[R, E, B] =
      ZIO.uninterruptibleMask { restore =>
        for {
          a <- acquire
          res <- use(a)
          _ <- release(a)
        } yield res
      }
  }

  trait Semaphore {
    def withPermits[R, E, A](n: Long)(task: ZIO[R, E, A]): ZIO[R, E, A]
  }

  object Semaphore {
    def make(permits: => Long): UIO[Semaphore] =
      for {
        permitsRef <- Ref.make(permits)
      } yield
        new Semaphore {
          override def withPermits[R, E, A](
            n: Long
          )(task: ZIO[R, E, A]): ZIO[R, E, A] = {
            for {
              result <- waitAndRun(n, task)
            } yield result
          }

          private def waitAndRun[R, E, A](neededPermits: Long,
                                          task: ZIO[R, E, A]) = {
            def loop(): ZIO[R, E, A] =
              permitsRef.get.flatMap {
                case available if available >= neededPermits =>
                  ZIO.acquireReleaseWith(
                    permitsRef.getAndAdd(0 - neededPermits).unit
                  )(
                    _ =>
                      permitsRef.getAndAdd(Math.max(0 + neededPermits, permits))
                  ) { _ =>
                    task
                  }
                case _ =>
                  ZIO.sleep(100.milliseconds) *> loop()
              }

            loop()
          }
        }
  }

  val testSemaphore: ZIO[Any, Nothing, Unit] = for {
    semaphore <- Semaphore.make(3)
    task = ZIO.logInfo("Task executed!")
    _ <- semaphore.withPermits(2)(task) // Should execute because 3 permits are available initially
  } yield ()

  // Test 2: This will hang forever.
  val testSemaphoreHang: ZIO[Any, Nothing, Unit] = for {
    semaphore <- Semaphore.make(1)
    task = ZIO.logInfo("Task executed!")
    _ <- semaphore.withPermits(2)(task) // Should wait until more permits are available
  } yield ()

  // Test 3: Ensure that a task can acquire the correct number of permits
  val testSemaphoreMultipleTasks: ZIO[Any, Nothing, Unit] = for {
    semaphore <- Semaphore.make(5)
    _ <- ZIO.foreachPar(0 to 10)(
      num => semaphore.withPermits(3)(ZIO.logInfo(s"Task ${num} executed!"))
    )
  } yield ()

  override def run = {
    testSemaphore *> testSemaphoreMultipleTasks
  }
}
