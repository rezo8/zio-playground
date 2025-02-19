package basics

import zio._

object CopyFile extends ZIOAppDefault {
  def run: ZIO[Any, Throwable, Unit] =
    for {
      contents <- ReadFile.readFileZio("test.txt")
      _ <- WriteFile.writeFileZio("example.txt", contents)
    } yield ()
}
