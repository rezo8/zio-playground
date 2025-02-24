package ch9to13_concurrent_structures.ch9

import zio.{Chunk, FiberRef, Ref, Scope, UIO, ZIO, ZIOAppDefault}

object RefExamples extends ZIOAppDefault {

  def increment(ref: Ref[Int]): UIO[Unit] =
    for {
      n <- ref.get
      _ <- ref.set(n + 1)
    } yield ()

  private val safeResult =
    for {
      ref1 <- Ref.make(0)
      ref2 <- Ref.make(0)
      _ <- increment(ref1)
      _ <- increment(ref2)
      _ <- increment(ref2)
      l <- ref1.get
      r <- ref2.get
    } yield (l, r)

  private val safeCount = for {
    ref <- Ref.make(0)
    _ <- ZIO.foreachParDiscard((1 to 10000).toList) { _ =>
      ref.update(_ + 1)
    }
    result <- ref.get
  } yield result

  private val unsafeCount = for {
    ref <- Ref.make(0)
    _ <- ZIO.foreachParDiscard((1 to 10000).toList) { _ =>
      ref.get.flatMap(n => ref.set(n + 1))
    }
    result <- ref.get
  } yield result

  final case class Tree[+A](head: A, tail: List[Tree[A]])
  type Log = Tree[Chunk[String]]

  val loggingRef: ZIO[Scope, Nothing, FiberRef[Log]] =
    FiberRef.make[Log](
      Tree(Chunk.empty, List.empty),
      _ => Tree(Chunk.empty, List.empty),
      (parent, child) => parent.copy(tail = child :: parent.tail)
    )

  def log(ref: FiberRef[Log])(string: String): UIO[Unit] =
    ref.update(log => log.copy(head = log.head :+ string))

  def logTree =
    for {
      ref <- loggingRef
      left = for {
        a <- ZIO.succeed(1).tap(_ => log(ref)("Got 1"))
        b <- ZIO.succeed(2).tap(_ => log(ref)("Got 2"))
      } yield a + b

      right = for {
        c <- ZIO.succeed(1).tap(_ => log(ref)("Got 3"))
        d <- ZIO.succeed(2).tap(_ => log(ref)("Got 4"))
      } yield c + d
      fiber1 <- left.fork
      fiber2 <- right.fork
      _ <- fiber1.join
      _ <- fiber2.join
      log <- ref.get
      _ <- ZIO.logInfo(log.toString)
    } yield ()

  override def run = {
    for {
      safe <- safeCount
      unsafe <- unsafeCount
      _ <- ZIO.logInfo(s"safe is ${safe}. unsafe is ${unsafe}")

      _ <- logTree
    } yield {
      ()
    }
  }
}
