package ch6_concurrency

import ch5_parallelism.CanFailFiber
import zio.http.URL
import zio.{ZIO, ZIOAppDefault}

object Exercises extends ZIOAppDefault {

  private def collectAllPar[R, E, A](
    in: Iterable[ZIO[R, E, A]]
  ): ZIO[R, E, List[A]] =
    ZIO.foreachPar(in.toList)(x => x)

  private def collectAllParResults[R, E, A](
    in: Iterable[ZIO[R, E, A]]
  ): ZIO[R, Nothing, (Iterable[E], Iterable[A])] =
    ZIO.partitionPar(in)(
      x => x.foldZIO(err => ZIO.fail(err), success => ZIO.succeed(success))
    )

  private def fetchUrl(url: String): ZIO[Any, Throwable, String] = {
    val shouldSucceed = scala.util.Random.nextBoolean()
    if (shouldSucceed) {
      ZIO.succeed(url)
    } else {
      ZIO.fail(new Throwable("Can't find url"))
    }
  }

  private def fetchAllUrlsPar(
    urls: List[String]
  ): ZIO[Any,
         Nothing,
         (Iterable[(String, Throwable)], Iterable[(String, String)])] = {
    ZIO.partitionPar(urls) { url =>
      fetchUrl(url)
        .foldZIO(err => ZIO.fail((url, err)), res => ZIO.succeed((url, res)))
    }
  }

  override def run = {
    val listOps = Range(0, 5).map(num => ZIO.logInfo(s"${num}"))
    val canFails = Range(0, 10).map(_ => CanFailFiber.canFail)
    for {
      _ <- ZIO.logInfo(s"Running collect all par")
      _ <- collectAllPar(listOps)
      _ <- ZIO.logInfo(s"Running collect all Par Results")
      results <- collectAllParResults(canFails)
      _ <- ZIO.logInfo(s"${results._1.size} number of failures")
      _ <- ZIO.logInfo(s"${results._2.size} number of successes")
      _ <- ZIO.logInfo(s"Running fetch all urls par")
      results2 <- fetchAllUrlsPar(Range(0, 10).map(_.toString).toList)
      _ <- ZIO.logInfo(s"$results2")
      _ <- ZIO.logInfo(s"${results2._1.size} number of failures")
      _ <- ZIO.logInfo(s"${results2._2.size} number of successes")
    } yield ()
  }
}
