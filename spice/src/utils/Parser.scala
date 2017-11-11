package utils

class Parser {

  def getParameters(element: String): Array[String] = {
  element.split(" ")

  }



}

object Parser extends Parser