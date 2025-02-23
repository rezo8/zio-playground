package ch7_structured_concurrency

import zio._
/*
  Important to note here that since we don't await,
  the child loops are interrupted.

  This is because the parent ends its execution and subsequently closes the child forks as,
   by starting them within its scope, it is in charge of managing them when it ends.

  Fork Daemon would make it so that the child would still execute outside of the scope of the parent.
   although not outside of the scope of the main.
 */
object StructuredConcurrency extends ZIOAppDefault {
  val parent: UIO[Unit] = {
    for {
      _ <- ZIO.debug("Parent fiber beginning execution...")
      _ <- child("foo").fork
      _ <- child("bar").fork
      _ <- ZIO.debug("Parent fiber ending execution...")
    } yield ()
  }.onExit(_ => ZIO.debug("Parent fiber exited!"))
  def child(name: String): UIO[Unit] =
    ZIO
      .debug(s"Child fiber $name is running...")
      .flatMap(_ => ZIO.sleep(5.seconds))
      .onInterrupt(ZIO.debug(s"Child fiber $name is interrupted!"))
  def run =
    for {
      _ <- ZIO.debug("Main fiber beginning execution...")
      _ <- parent.fork
      _ <- ZIO.debug("Main fiber ending execution...")
    } yield ()
}
