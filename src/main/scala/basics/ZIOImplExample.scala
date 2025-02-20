package basics

import basics.ZIOToy.ZIOImpl
import zio.{Runtime, Unsafe, ZIO}
object ZIOImplExample extends App {
  val zio1: ZIOImpl[Any, String, Int] = ZIOImpl(_ => Right(42))

  // Define another ZIO effect that succeeds with an Int value
  val zio2: ZIOImpl[Any, String, Int] = ZIOImpl(_ => Right(100))

  val zioFail: ZIOImpl[Any, String, Int] = ZIOImpl(_ => Left("100"))

  // Combine the results of zio1 and zio2 using zipWith
  val zioCombined: ZIOImpl[Any, String, Int] = ZIOToy.zipWith(zio1, zio2)(_ + _)

  // Run the combined effect
  println(zioCombined.run(()))
  println(ZIOToy.collectAll(List(zio1, zioFail, zio2)).run(()))

  // Should just be right
  println(ZIOToy.forEach(List(zio1, zio2)).run(()))

  // Should just be left.
  println(ZIOToy.forEach(List(zio1, zioFail, zio2)).run(()))

  // Should be zio2 success
  println(ZIOToy.orElse(zioFail, zio2).run(()))

  // should be zio1 success
  println(ZIOToy.orElse(zio1, zioFail).run(()))

  val runtime = Runtime.default

  val failure = for {
    _ <- ZIOToy.eitherToZIO(Right("Test"))
    _ <- ZIOToy.eitherToZIO(Left("Test"))
    y <- ZIOToy.eitherToZIO(Right("unreachable"))
  } yield println(s"unexpected success at ${y}")

  Unsafe.unsafe { implicit unsafe =>
    runtime.unsafe
      .run(ZIOToy.eitherToZIO(Right("Test")))
      .getOrThrowFiberFailure()

    runtime.unsafe
      .run(failure)
      .getOrElse(_ => println("successfully failed"))
  }

  Unsafe.unsafe { implicit unsafe =>
    println(
      runtime.unsafe
        .run(ZIOToy.listToZIO(List(1)))
        .getOrThrowFiberFailure()
    )

    runtime.unsafe
      .run(ZIOToy.listToZIO(List()))
      .getOrElse(_ => println("successfully failed"))
  }

  // Print 2 different times
  Unsafe.unsafe { implicit unsafe =>
    println(
      runtime.unsafe
        .run(ZIOToy.currentTimeZIO)
        .getOrThrowFiberFailure()
    )

    Thread.sleep(1000)

    println(
      runtime.unsafe
        .run(ZIOToy.currentTimeZIO)
        .getOrThrowFiberFailure()
    )
  }

  // Print 2 different times
  Unsafe.unsafe { implicit unsafe =>
    println(runtime.unsafe.run(ZIOToy.getCacheValueZio("foo")).getOrThrow())
    println(
      runtime.unsafe
        .run(
          ZIOToy
            .getCacheValueZio("dne")
        )
        .getOrElse(_ => "successfully failed get cache value")
    )
  }
}
