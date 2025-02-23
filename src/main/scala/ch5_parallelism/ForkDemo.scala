package ch5_parallelism

import zio.{UIO, ZIO, ZIOAppDefault}

object ForkDemo {
  lazy private val doSomething: UIO[Unit] = ???
  lazy private val doSomethingElse: UIO[Unit] = ???

  /*
  Here doSomething is guaranteed to happen before doSomethingElse.
   */
  lazy private val example1 = for {
    _ <- doSomething
    _ <- doSomethingElse
  } yield ()

  /*
  Here doSomething is forked away the order of exec is no longer guaranteed.
  Fork does not even wait for forked fiber to begin before returning.
   */
  lazy private val example2 = for {
    _ <- doSomething.fork
    _ <- doSomethingElse
  } yield ()

}
