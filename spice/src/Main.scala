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


import settings.Settings.{maximumElements, maximumName, maximumNodes, tolg, version}
import utils.{FileHandler, Parser}
import protocol.Protocols.Element

import scala.collection.mutable.ListBuffer
import sys.process._

/**  === Elements indexing ===
  *  Resistor:                            R <name> <n+> <n-> <R>
  *  Inductor:                            L <name> <n+> <n-> <L> <InitialCurrent>
  *  Capacitor:                           C <name> <n+> <n-> <C> <InitialVoltage>
  *  Voltage controlled by current:       G <name> <io+> <io-> <vi+> <vi-> <G>
  *  Voltage controlled by voltage:       E <name> <vo+> <vo-> <vi+> <vi-> <Av>
  *  Current controlled by current:       F <name> <io+> <io-> <ii+> <ii-> <Ai>
  *  Current controlled by voltage:       H <name> <vo+> <vo-> <ii+> <ii-> <R>
  *  Independent current source:          I <name> <io+> <io-> <I>
  *  Independent voltage source:          V <name> <vo+> <vo-> <V>
  *  Ideal Operational Amplifier:         O <name> <vo1> <vo2> <vi1> <vi2>
  *  Resistor linear by parts:            N <name> <n+> <n-> <point1> <point2> <point3> <point4>
  *  Ideal Transformer:                   K <name> <nA> <nB> <nC> <nD> <n>
  *  Switch:                              $ <name> <nA> <nB> <nControlC> <nControlD> <GOn> <GOff> <VRef>
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

  /**
    *   Initialization of var's
    */

  var netList = ListBuffer[Element]()

  val list: Array[String] = new Array[String](maximumName)


  var variableIterator = 0
  var elementIterator = 0
  var nodeIterator = 0
  /**
    * =================
    */

  /**
    * Analysis parameter
    */
  var totalTime : Double = _
  var stepSize : Double = _
  var stepsPerPoint : Double = _
  /**
    * ==================
    */

    //TODO Check if really starts with null value || Init matrix need
  val admittanceMatrix = Array.ofDim[Double](maximumNodes+1, maximumNodes+2)

  listOfElements.foreach(element => {
    elementIterator += 1 //don't use netList(0)
    element(0).toUpper match {
      case x if x == 'R' || x == 'I' || x == 'V' =>
        netList(elementIterator).`type` += element(0)
        netList(elementIterator).name += Parser.getParameters(element)(0)
        netList(elementIterator).nodes.nodeA += Number(Parser.getParameters(element)(1))
        netList(elementIterator).nodes.nodeB += Number(Parser.getParameters(element)(2))
        netList(elementIterator).values.head += Parser.getParameters(element)(3)

      case x if x == 'L' || x == 'C'=>
        netList(elementIterator).`type` += element(0)
        netList(elementIterator).name += Parser.getParameters(element)(0)
        netList(elementIterator).nodes.nodeA += Number(Parser.getParameters(element)(1))
        netList(elementIterator).nodes.nodeB += Number(Parser.getParameters(element)(2))
        netList(elementIterator).values.head += Parser.getParameters(element)(3)
        netList(elementIterator).values(1) += Parser.getParameters(element)(4) // Initial value

      case x if x == 'G' || x == 'E' || x == 'F' || x == 'H' =>
        netList(elementIterator).`type` += element(0)
        netList(elementIterator).name += Parser.getParameters(element)(0)
        netList(elementIterator).nodes.nodeA += Number(Parser.getParameters(element)(1))
        netList(elementIterator).nodes.nodeB += Number(Parser.getParameters(element)(2))
        netList(elementIterator).nodes.nodeC += Number(Parser.getParameters(element)(3))
        netList(elementIterator).nodes.nodeD += Number(Parser.getParameters(element)(5))
        netList(elementIterator).values.head += Parser.getParameters(element)(6)

      case x if x == 'O' =>
        netList(elementIterator).`type` += element(0)
        netList(elementIterator).name += Parser.getParameters(element)(0)
        netList(elementIterator).nodes.nodeA += Number(Parser.getParameters(element)(1))
        netList(elementIterator).nodes.nodeB += Number(Parser.getParameters(element)(2))
        netList(elementIterator).nodes.nodeC += Number(Parser.getParameters(element)(3))
        netList(elementIterator).nodes.nodeD += Number(Parser.getParameters(element)(5))

      case '$' =>


      case x if x == '*' =>
        println(s"Comment: $element")
        elementIterator -= 1

      case '.' =>
        totalTime = Parser.getParameters(element)(1).toDouble
        stepSize = Parser.getParameters(element)(2).toDouble
        stepsPerPoint = Parser.getParameters(element)(4).toDouble
        elementIterator -= 1

      case _ =>
        println(s"Unknown element")
        elementIterator -= 1
    }


  })

  nodeIterator = variableIterator

  // Add current variables above nodes
  for(idx <- 1 to elementIterator){
    netList(elementIterator).`type`.head match {
      case x if x == 'V' || x == 'E' || x == 'F' || x == 'O' =>
        variableIterator += 1
        if(variableIterator > maximumNodes){
          println(s"Extra current exceed number of variables allowed, that is $maximumNodes nodes.")
          sys.exit(1)
        }
        list(variableIterator) = "j" + netList(idx).name

      case x if x == 'H' =>
        variableIterator += 2
        if(variableIterator > maximumNodes){
          println(s"Extra current exceed number of variables allowed, that is $maximumNodes nodes.")
          sys.exit(1)
        }
        list(variableIterator-1) = "jx" + netList(idx).name
        netList(idx).nodes.nodeX += variableIterator - 1
        list(variableIterator) = "jy" + netList(idx).name
        netList(idx).nodes.nodeY += variableIterator
    }
  }

  println("====================================================================================================\n")
  println(s"The circuit has -> $nodeIterator nodes, $variableIterator variables and $elementIterator elements.\n")
  println("====================================================================================================")



  def Number(nameElement: String): Int = {
    var idx: Int = 0
    val found: Boolean = false

    while (!found && idx <= variableIterator){
      if(found == nameElement.equalsIgnoreCase(list(idx))){
        idx += 1
      }}
      if(!found){
        if(variableIterator == maximumNodes){
          println(s"The program just accept $maximumNodes nodes.")
          sys.exit(1)
        }
        variableIterator+=1
        list(variableIterator) = nameElement
        variableIterator // New node
      }
      else {
        idx // Node already named
      }
    }


  //TODO Make sure that using TO or UNTIL and STEP correctly
   def Solve(): Unit = {
    var idxA: Int = 0

    var idxT: Double = 0.0
    var idxP: Double = 0

    for (idxI <- 1 to variableIterator){
      idxA = idxI
      for(idxL <- idxI to variableIterator){
        if(Math.abs(admittanceMatrix(idxL)(idxI)) > Math.abs(idxT)){
          idxA = idxL
          idxT = admittanceMatrix(idxL)(idxI)
        }
      }
      if(idxI != idxA){
        for(idxL <- 1 to variableIterator){
          idxP = admittanceMatrix(idxI)(idxL)
          admittanceMatrix(idxI)(idxL) = admittanceMatrix(idxA)(idxL)
          admittanceMatrix(idxA)(idxL) = idxP
        }
      }
      if(Math.abs(idxT) < tolg){
        println("Singular system")
        sys.exit(1)
      }
      for(idxJ <- variableIterator+1 to (0, -1)){
        admittanceMatrix(idxI)(idxJ) /= idxT //TODO Check
        idxP=admittanceMatrix(idxI)(idxJ)
        if(idxP != 0){
          for (idxL <- 1 to variableIterator){
            if(idxL != idxI){
              admittanceMatrix(idxL)(idxI) -= admittanceMatrix(idxL)(idxI)*idxP
            }
          }
        }
      }
    }
  }

  //TODO How L may have X node?
  def BuildStamps(): Unit ={
    for (idx <- 1 to elementIterator){
      netList(idx).`type` match {
        case x if x == "L" =>
          //Numeric integration must deliver G and Z
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeA) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeB) += 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeX) += g
          admittanceMatrix(netList(idx).nodes.nodeX)(variableIterator+1) += z

        case x if x == "C" =>
          //Numeric integration must deliver G and Z
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeA) += g
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeB) += g
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeB) -= g
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeA) -= g
          admittanceMatrix(netList(idx).nodes.nodeA)(variableIterator+1) += z
          admittanceMatrix(netList(idx).nodes.nodeB)(variableIterator+1) -= z

        case x if x == "R" =>
          val conductance = 1/netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeA) += conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeB) += conductance
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeB) -= conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeA) -= conductance

        case x if x == "G" =>
          val conductance = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeC) += conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeD) += conductance
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeD) -= conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeC) -= conductance

        case x if x == "I" =>
          val current = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(variableIterator+1) -= current
          admittanceMatrix(netList(idx).nodes.nodeB)(variableIterator+1) += current

        case x if x == "V" =>
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeA) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeB) += 1

        case x if x == "E" =>
          val conductance = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeA) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeB) += 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeC) += conductance
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeD) -= conductance

        case x if x == "F" =>
          val conductance = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= conductance
          admittanceMatrix(netList(idx).nodes.nodeC)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeD)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeC) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeD) += 1

        case x if x == "H" =>
          val conductance = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeY) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeY) -= 1
          admittanceMatrix(netList(idx).nodes.nodeC)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeD)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeY)(netList(idx).nodes.nodeA) -= 1
          admittanceMatrix(netList(idx).nodes.nodeY)(netList(idx).nodes.nodeB) += 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeC) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeD) += 1
          admittanceMatrix(netList(idx).nodes.nodeY)(netList(idx).nodes.nodeX) += conductance

        case x if x == "$" =>

        case x if x == "O" =>
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeC) += 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeD) -= 1

        case x if x == "N" =>

        case x if x == "DC" =>
          admittanceMatrix(netList(idx).nodes.nodeX)(variableIterator+1) -= netList(idx).values.head

        case x if x == "SIN" =>

        case x if x == "PULSE" =>

      }
    }
  }





}