package basics

import zio.{Task, ZIO}

import scala.io.StdIn

object ConsoleOps {
  def readLine: Task[String] = ZIO.attempt(StdIn.readLine())
  def printLine(line: String): Task[Unit] = ZIO.attempt(println(line))
}
