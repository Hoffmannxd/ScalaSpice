package utils


import scala.io.Source

class FileHandler {

  def openFile(fileName: String): List[String] = {
      Source.fromFile(fileName).getLines.toList
  }

}

object FileHandler extends FileHandler