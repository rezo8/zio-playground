package basics

import zio.*

import scala.io.Source

object ReadFile {

  private def readFile(file: String): String = {
    val source = Source.fromResource(file)
    try source.getLines().mkString
    finally source.close()
  }

  def readFileZio(file: String): ZIO[Any, Throwable, String] =
    for {
      fileContents <- ZIO.attempt(readFile(file))
    } yield fileContents
}
