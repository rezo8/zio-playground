package ch14to16_resource_management.ch15_composable_resources

import zio.*

import java.net.Socket
import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try}

object Example extends ZIOAppDefault {

  def worker(sem: Semaphore, id: Int): ZIO[Scope, Nothing, Unit] =
    for {
      _ <- sem.withPermitsScoped(2)
      _ <- Console.printLine(s"Request $id: Starting processing ").orDie
      _ <- ZIO.sleep(1.seconds)
      _ <- Console.printLine(s"Request $id: Completed processing ").orDie
    } yield ()

  /**
    * Explain the difference between the following 2 mains.
    */
  /*
      Why do they behave different?
        - MainApp1 creates a scope for each worker, which discards its permit once the scope completes.
            Thus the other workers can get the work from the semaphore, as when the work completes the permit refreshes.
        - MainApp2 has a scope around all the workers, so since the scope around it never ends, it hangs on the first 2 workers.
   */

  object MainApp1 extends ZIOAppDefault {
    def run =
      for {
        sem <- Semaphore.make(4)
        _ <- ZIO.foreachParDiscard(1 to 10)(i => ZIO.scoped(worker(sem, i)))
      } yield ()
  }

  object MainApp2 extends ZIOAppDefault {
    def run =
      for {
        sem <- Semaphore.make(6)
        _ <- ZIO.scoped(ZIO.foreachParDiscard(1 to 10)(i => worker(sem, i)))
      } yield ()
  }

  trait Cache[K, V] {
    def put(key: K, value: V, ttl: Duration): UIO[Unit]
    def get(key: K): UIO[Option[V]]
  }

  object Cache {
    def make[K, V](
      invalidationInterval: Duration = 1.seconds
    ): ZIO[Scope, Nothing, Cache[K, V]] = {
      for {
        inMemMapRef <- Ref.make(Map[K, (V, Long)]())

      } yield
      // TODO this does not handle collisions. To fix we should put expiry date on key.
      new Cache[K, V] {
        override def put(key: K, value: V, ttl: Duration): UIO[Unit] = {
          val expiration = Instant
            .now()
            .plusSeconds(ttl.toSeconds)
            .toEpochMilli
          for {
            _ <- inMemMapRef.update(_.updated(key, (value, expiration)))
            _ <- (for {
              _ <- ZIO.logInfo("sleeping for ttl")
              _ <- ZIO.sleep(ttl)
              _ <- inMemMapRef.update(x => {
                x.get(key)
                  .fold(x)(currValue => {
                    if (currValue._2 < Instant.now().toEpochMilli) {
                      x
                    } else {
                      x.removed(key)
                    }
                  })
              })
            } yield ()).fork
          } yield ()
        }

        override def get(key: K): UIO[Option[V]] = {
          inMemMapRef.get.map(_.get(key).map(_._1))
        }
      }
    }
  }

  def cacheExample =
    ZIO.scoped {
      for {
        cache <- Cache.make[String, Int]()
        _ <- cache.put("key1", 100, 5.seconds)
        _ <- cache.put("key2", 200, 5.seconds)
        v1 <- cache.get("key1")
        v2 <- cache.get("key2")
        _ <- ZIO.logInfo(s"Initial value for key1: $v1, key2: $v2")
        _ <- cache.put("key2", 300, 8.seconds)
        _ <- ZIO.sleep(5.seconds)
        v2 <- cache.get("key1") // Should be None (expired)
        v3 <- cache.get("key2") // Should still exist with new value
        _ <- ZIO.logInfo(s"After 5s - key1: $v2, key2: $v3")
        _ <- ZIO.sleep(10.seconds)
        v2 <- cache.get("key1") // Should be None (expired)
        v3 <- cache.get("key2") // Should be None (expired)
        _ <- ZIO.logInfo(s"After 15s - key1: $v2, key2: $v3")
      } yield ()
    }

  override def run = {
//    testSemaphore *> testSemaphoreMultipleTasks
    cacheExample *> MainApp1.run *> ZIO.logInfo("MainApp2 Running") *> MainApp2.run
  }
}
