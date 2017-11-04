import settings.Settings.{version, maximumElements}
import utils.FileHandler

import sys.process._


object Main extends App {

  val clear = "clear" !
  //val clear = "cls" !

  println("EEL525 - Electric Circuits II")
  println("Developed by Matheus Hoffmann - hoffmann@poli.ufrj.br")
  println("Circuit Analysis in the time domain containing liner/nonlinear components")
  println(s"Version -> $version")
  println("Insert NetList file (example.net):")

  /**
    * The file must be in the context path, not jar path
    */
  val fileNetList = scala.io.StdIn.readLine()

  val numberOfElements = FileHandler.openFile(fileNetList).size

  if (numberOfElements > maximumElements) {
    println(s"Max elements reached, please enter a NetList with less than $maximumElements elements.")
    sys.exit(1)
  }



}