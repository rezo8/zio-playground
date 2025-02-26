package ch9to13_concurrent_structures.ch10

import zio.*

import java.util

object Exercises extends ZIOAppDefault {

  trait CountDownLatch {
    def countDown: UIO[Unit]
    def await: UIO[Unit]
  }

  object CountDownLatch {
    def make(n: Int): UIO[CountDownLatch] = {
      for {
        ref <- Ref.make(n)
        promise <- Promise.make[Nothing, Unit]
      } yield
        new CountDownLatch {
          override def countDown: UIO[Unit] =
            ref
              .modify(
                x =>
                  if (x > 1) {
                    (ZIO.unit, x - 1)
                  } else {
                    (promise.succeed(()).unit, 0)
                }
              )
              .flatten

          override def await: UIO[Unit] = {
            promise.await
          }
        }
    }
  }

  val countdownProgram = for {
    latch <- CountDownLatch.make(3) // Initialize the latch with a count of 3
    _ <- ZIO.foreachPar(1 to 3) { i =>
      // Simulate some work
      ZIO.sleep(1.second) *>
        ZIO.logInfo(s"Task $i completed") *>
        latch.countDown // Decrement the latch count
    }
    _ <- ZIO.logInfo("Waiting for all tasks to complete...")
    _ <- latch.await // Wait for the latch to reach zero
    _ <- ZIO.logInfo("All tasks completed!")
  } yield ExitCode.success

  trait CyclicBarrier {
    def await: UIO[Unit]
    def reset: UIO[Unit]
  }

  object CyclicBarrier {
    def make(parties: Int): UIO[CyclicBarrier] = {
      for {
        ref <- Ref.make(parties) // Ref to track the number of parties remaining
        promise <- Promise
          .make[Nothing, Unit] // Promise to signal when the barrier is released
        resetPromise <- Promise.make[Nothing, Unit] // Promise to handle reset
      } yield
        new CyclicBarrier {

          override def await: UIO[Unit] = {
            for {
              remaining <- ref.modify { remaining =>
                if (remaining > 1) {
                  (ZIO.unit, remaining - 1)
                } else {
                  (promise.succeed(()).unit, 0)
                }
              }.flatten
              _ <- promise.await
              _ <- resetPromise.succeed(())
            } yield ()
          }

          override def reset: UIO[Unit] = {
            for {
              _ <- ref.set(parties)
              _ <- resetPromise.await *> promise.completeWith(
                resetPromise.poll.map(_.getOrElse(ZIO.unit))
              )
            } yield ()
          }
        }
    }
  }

  private val cyclicBarrierProgram = for {
    barrier <- CyclicBarrier.make(3) // Initialize the barrier with 3 parties
    _ <- ZIO.foreachPar(1 to 3) { i =>
      for {
        _ <- ZIO.logInfo(s"Task $i is working...")
        _ <- ZIO.sleep(1.second) // Simulate some work
        _ <- ZIO.logInfo(s"Task $i reached the barrier")
        _ <- barrier.await // Wait at the barrier
        _ <- ZIO.logInfo(s"Task $i passed the barrier")
      } yield ()
    }
    _ <- ZIO.logInfo("All tasks passed the barrier!")
  } yield ExitCode.success

  trait Queue[A] {
    def offer(a: A): UIO[Unit]
    def take: UIO[A]
  }

  object Queue {
    def make[A](capacity: Int): UIO[Queue[A]] =
      for {
        ref <- Ref.make(Vector.empty[A])
        sizeRef <- Ref.make(0)
        offerPromise <- Promise.make[Nothing, Unit]
        takePromise <- Promise.make[Nothing, A]
      } yield
        new Queue[A] {
          override def offer(a: A): UIO[Unit] = {
            for {
              size <- sizeRef.get
              _ <- if (size < capacity) {
                // There is space.
                for {
                  _ <- ref.update(_ :+ a)
                  _ <- sizeRef.getAndAdd(1)
                  _ <- takePromise.succeed(a).unit
                } yield ()
              } else {
                // No space. We need to await.
                for {
                  _ <- offerPromise.await
                  _ <- offer(a) // recursively call after we can offer
                } yield ()
              }
            } yield ()
          }

          override def take: UIO[A] = {
            for {
              size <- sizeRef.get
              item <- if (size == 0) {
                // No way to take.
                for {
                  _ <- takePromise.await
                  item <- take
                } yield item
              } else {
                // We can take.
                for {
                  items <- ref.get
                  toTake <- ref.modify(vector => {
                    (vector.head, vector.drop(1))
                  })
                  _ <- sizeRef.getAndAdd(-1)
                  _ <- offerPromise.succeed(()).unit
                } yield toTake
              }
            } yield item
          }
        }
  }

  private val queueProgram = for {
    queue <- Queue.make[Int](2) // Create a queue with capacity 2
    producer <- ZIO
      .foreachPar(1 to 5) { i =>
        for {
          _ <- ZIO.logInfo(s"Producing $i")
          _ <- queue.offer(i) // Offer items to the queue
          _ <- ZIO.logInfo(s"Produced $i")
        } yield ()
      }
      .fork // Run the producer in a separate fiber
    consumer <- ZIO
      .foreachPar(1 to 5) { _ =>
        for {
          item <- queue.take // Take items from the queue
          _ <- ZIO.logInfo(s"Consumed $item")
        } yield ()
      }
      .fork // Run the consumer in a separate fiber
    _ <- producer.join
    _ <- consumer.join
  } yield ExitCode.success

  override def run = {
    for {
      _ <- ZIO.logInfo("COUNTDOWN")
      _ <- countdownProgram
      _ <- ZIO.logInfo("CYCLIC BARRIER")
      _ <- cyclicBarrierProgram
      _ <- ZIO.logInfo("QUEUE")
      _ <- queueProgram
    } yield 1
  }
}
