package ch9to13_concurrent_structures.ch9

import zio.*

object Exercises extends ZIOAppDefault {

  override def run = {
    for {
      _ <- counterApp
      _ <- boundedApp
    } yield ()
  }

  private val boundedApp = {
    for {
      queue <- BoundedQueue.make[Int](3) // Create a queue with capacity 3
      _ <- queue
        .enqueue(1)
        .flatMap(result => ZIO.logInfo(s"Enqueued 1: $result"))
      _ <- queue
        .enqueue(2)
        .flatMap(result => ZIO.logInfo(s"Enqueued 2: $result"))
      _ <- queue
        .enqueue(3)
        .flatMap(result => ZIO.logInfo(s"Enqueued 3: $result"))
      _ <- queue
        .enqueue(4)
        .flatMap(result => ZIO.logInfo(s"Enqueued 4: $result")) // This will fail (queue is full)
      _ <- queue.size.flatMap(size => ZIO.logInfo(s"Queue size: $size"))
      _ <- queue.dequeue.flatMap {
        case Some(value) => ZIO.logInfo(s"Dequeued: $value")
        case None        => ZIO.logInfo("Dequeued: None")
      }
      _ <- queue.dequeue.flatMap {
        case Some(value) => ZIO.logInfo(s"Dequeued: $value")
        case None        => ZIO.logInfo("Dequeued: None")
      }
      _ <- queue.size.flatMap(size => ZIO.logInfo(s"Queue size: $size"))
      _ <- queue.dequeue.flatMap {
        case Some(value) => ZIO.logInfo(s"Dequeued: $value")
        case None        => ZIO.logInfo("Dequeued: None")
      }
      _ <- queue.dequeue.flatMap {
        case Some(value) => ZIO.logInfo(s"Dequeued: $value")
        case None        => ZIO.logInfo("Dequeued: None")
      }
      _ <- queue.size.flatMap(size => ZIO.logInfo(s"Queue size: $size"))
    } yield ()
  }
  trait BoundedQueue[A] {
    def enqueue(a: A): UIO[Boolean]
    def dequeue: UIO[Option[A]]
    def size: UIO[Int]
    def capacity: UIO[Int]
  }

  private object BoundedQueue {
    def make[A](bound: Int): ZIO[Any, Nothing, BoundedQueue[A]] = {
      Ref.make(List.empty[A]).map(x => BoundedQueueImpl[A](bound, x))
    }
  }

  class BoundedQueueImpl[A](bound: Int, ref: Ref[List[A]])
      extends BoundedQueue[A] {
    override def enqueue(a: A): UIO[Boolean] =
      ref.modify { list =>
        if (list.size < bound) (true, a :: list)
        else (false, list)
      }
    override def dequeue: UIO[Option[A]] =
      ref.modify {
        case Nil => (None, Nil) // Return None if the queue is empty
        case list =>
          (Some(list.last), list.init) // Remove and return the last element
      }
    override def size: UIO[Int] = ref.get.map(_.size)
    override def capacity: UIO[Int] = size.map(bound - _)
  }

  private val counterApp = {
    for {
      counter <- Counter.make(0)
      _ <- counter.increment.flatMap(x => ZIO.logInfo(x.toString))
      _ <- counter.increment.flatMap(x => ZIO.logInfo(x.toString))
      _ <- counter.increment.flatMap(x => ZIO.logInfo(x.toString))
      _ <- counter.decrement.flatMap(x => ZIO.logInfo(x.toString))
      _ <- counter.get.flatMap(x => ZIO.logInfo(x.toString))
      _ <- counter.reset.flatMap(_ => ZIO.logInfo("reset"))
      _ <- counter.get.flatMap(x => ZIO.logInfo(x.toString))
    } yield ()
  }

  trait Counter {
    def increment: UIO[Long]
    def decrement: UIO[Long]
    def get: UIO[Long]
    def reset: UIO[Unit]
  }

  private object Counter {
    def make(initialValue: Long): UIO[Counter] =
      Ref
        .make(initialValue)
        .map(
          ref =>
            new Counter {
              override def increment: UIO[Long] = ref.updateAndGet(_ + 1)
              override def decrement: UIO[Long] = ref.updateAndGet(_ - 1)
              override def get: UIO[Long] = ref.get
              override def reset: UIO[Unit] = ref.set(initialValue)
          }
        )
  }

  type CounterId = String

  trait CounterManager {
    def increment(id: CounterId): UIO[Long]
    def decrement(id: CounterId): UIO[Long]
    def get(id: CounterId): UIO[Long]
    def reset(id: CounterId): UIO[Unit]
    def remove(id: CounterId): UIO[Unit]
  }

  object CounterManager {
    def make: UIO[CounterManager] =
      Ref.Synchronized
        .make(Map.empty[CounterId, Ref[Counter]])
        .map(
          ref =>
            new CounterManager {
              override def increment(id: CounterId): UIO[Long] = {
                applyToCounter[Long](id)(counter => counter.increment)
              }

              override def decrement(id: CounterId): UIO[Long] = {
                applyToCounter[Long](id)(counter => counter.decrement)
              }

              override def get(id: CounterId): UIO[Long] =
                applyToCounter[Long](id)(counter => counter.get)

              override def reset(id: CounterId): UIO[Unit] =
                applyToCounter[Unit](id)(counter => counter.reset)

              override def remove(id: CounterId): UIO[Unit] =
                ref.modifyZIO(map => {
                  ZIO.succeed((), map.removed(id))
                })

              def applyToCounter[A](
                id: CounterId
              )(f: Counter => UIO[A]): UIO[A] = {
                ref.modifyZIO(map => {
                  map.get(id) match {
                    case Some(value) =>
                      for {
                        counter <- value.get
                        currVal <- f(counter)
                      } yield (currVal, map)
                    case None =>
                      for {
                        counter <- Counter.make(0L)
                        currVal <- f(counter)
                        ref <- Ref.make(counter)
                      } yield (currVal, map + (id -> ref))
                  }
                })
              }
          }
        )
  }
}
