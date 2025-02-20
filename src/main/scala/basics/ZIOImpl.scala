package basics

import zio.ZIO
import zio.*

import java.io.IOException
import scala.concurrent.{ExecutionContext, Future}

object ZIOToy {
  final case class ZIOImpl[-R, +E, +A](run: R => Either[E, A])

  def zipWith[R, E, A, B, C](self: ZIOImpl[R, E, A], that: ZIOImpl[R, E, B])(
    func: (A, B) => C
  ): ZIOImpl[R, E, C] = {
    ZIOImpl { (r: R) =>
      {
        for {
          res1 <- self.run(r)
          res2 <- that.run(r)
        } yield func(res1, res2)
      }
    }
  }

  def collectAll[R, E, A](
    in: Iterable[ZIOImpl[R, E, A]]
  ): ZIOImpl[R, E, List[Either[E, A]]] =
    ZIOImpl { (r: R) =>
      {
        Right(in.map(_.run(r)).toList)
      }
    }

  def forEach[R, E, A](in: Iterable[ZIOImpl[R, E, A]]): ZIOImpl[R, E, List[A]] =
    ZIOImpl { (r: R) =>
      {
        in.foldLeft[Either[E, List[A]]](Right(List.empty[A])) { (acc, zio) =>
          for {
            list <- acc // Accumulated list so far
            a <- zio.run(r) // Run the current ZIO effect
          } yield list :+ a // Append the result to the list
        }
      }
    }

  def orElse[R, E1, E2, A](self: ZIOImpl[R, E1, A],
                           that: ZIOImpl[R, E2, A]): ZIOImpl[R, E2, A] =
    ZIOImpl { (r: R) =>
      {
        self.run(r).fold(err => that.run(r), res => Right[E2, A](res))
      }
    }

  // Use regular ZIO
  def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] =
    either.fold(err => ZIO.fail(err), success => ZIO.succeed(success))

  // Use regular ZIO
  def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] = {
    list.headOption.fold(ZIO.fail(None))(success => ZIO.succeed(success))
  }

  lazy val currentTimeZIO: ZIO[Any, Nothing, Long] =
    ZIO.succeed(java.lang.System.currentTimeMillis())

  private val simCache = Map("foo" -> "bar")
  def getCacheValue(key: String,
                    onSuccess: String => Unit,
                    onFailure: Throwable => Unit): Unit =
    simCache
      .get(key)
      .fold(onFailure(new Throwable("missing key")))(
        success => onSuccess(success)
      )

  def getCacheValueZio(key: String): ZIO[Any, Throwable, String] =
    ZIO.async { callback =>
      getCacheValue(
        key,
        value => callback(ZIO.succeed(value)), // Success case
        error => callback(ZIO.fail(error)) // Failure case
      )
    }

  def doQueryZio[A](
    query: Future[A]
  )(implicit ec: ExecutionContext): ZIO[Any, Throwable, A] =
    ZIO.fromFuture(ec => query)

}
