package basics

import zio.*

object WriteFile {
  private def writeFile(file: String, text: String): Unit = {
    import java.io.*
    val pw = new PrintWriter(new File(file))
    try pw.write(text)
    finally pw.close()
  }

  def writeFileZio(file: String, text: String): Task[Unit] =
    ZIO.attempt(writeFile(file, text))

}
