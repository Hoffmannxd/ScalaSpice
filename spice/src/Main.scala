/**
    _________             .__             _________      .__
   /   _____/ ____ _____  |  | _____     /   ___________ |__| ____  ____
   \_____  \_/ ___\\__  \ |  | \__  \    \_____  \\____ \|  _/ ____/ __ \
   /        \  \___ / __ \|  |__/ __ \_  /        |  |_> |  \  \__\  ___/
  /_______  /\___  (____  |____(____  / /_______  |   __/|__|\___  \___  >
          \/     \/     \/          \/          \/|__|           \/    \/

Scala Spice - Circuit simulator using Scala
Matheus Hoffmann Fernandes Santos - hoffmann@poli.ufrj.br

GitHub: https://github.com/Hoffmannxd/scala-spice
*/

import settings.Settings.{maximumElements, version}
import utils.FileHandler
import protocol.Protocols.Element

import scala.collection.mutable.ListBuffer
import sys.process._

/**  === Elements indexing ===
  *  Resistor:                            R <name> <no+> <no-> <R>
  *  Voltage controlled by current:       G <name> <io+> <io-> <vi+> <vi-> <G>
  *  Voltage controlled by voltage:       E <name> <vo+> <vo-> <vi+> <vi-> <Av>
  *  Current controlled by current:       F <name> <io+> <io-> <ii+> <ii-> <Ai>
  *  Current controlled by voltage:       H <name> <vo+> <vo-> <ii+> <ii-> <R>
  *  Independent current source:          I <name> <io+> <io-> <I>
  *  Independent voltage source:          V <name> <vo+> <vo-> <V>
  *  Operational Amplifier:               O <name> <vo1> <vo2> <vi1> <vi2>
  *
  *  === Sources indexing ===
  *  Direct current source:               DC    <value>
  *  Sinusoidal source:                   SIN   <VOffset> <V> <freq> <delay> <dumping> <phase> <cycles>
  *  Pulse source:                        PULSE <v1> <v2> <delay> <tRise> <tFall> <tOn> <T> <cycles>
  *
  *  === Analysis Parameters ===
  *  .TRAN <totalTime> <step> TRAP <stepsPerPoint>
  *
  * Remember input file must be in context path
  */
object Main extends App {

  val clear = "clear" !
  //val clear = "cls" !

  println("EEL525 - Electric Circuits II")
  println("Developed by Matheus Hoffmann - hoffmann@poli.ufrj.br")
  println("Circuit Analysis in the time domain containing liner/nonlinear components")
  println(s"Version -> $version")
  println("Insert NetList file (example.net):")

  val fileNetList = scala.io.StdIn.readLine()

  val listOfElements = FileHandler.openFile(fileNetList)

  val numberOfElements = listOfElements.size

  if (numberOfElements > maximumElements) {
    println(s"Maximum of elements reached, please enter a NetList with less than $maximumElements elements.")
    sys.exit(1)
  }

  val netList = ListBuffer[Element]()
  var elementIterator = 0

  listOfElements.foreach(element => {
    elementIterator += 1
    element(0).toUpper match {
      case x if x == 'R' || x == 'I' || x == 'V' =>

      case x if x == 'G' || x == 'E' || x == 'F' || x == 'H' =>

      case x if x == 'O' =>

      case x if x == '*' =>
        println(s"Comment: $element")
        elementIterator -= 1

      case _ =>
        println(s"Unknown element")

    }


  })


}