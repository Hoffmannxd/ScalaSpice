package main

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


import settings.Settings._

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.sys.process._

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

  /**
    * Structure
    */
  case class NodeType(nodeA: Int, nodeB: Int, nodeC: Int, nodeD: Int, nodeX: Int, nodeY: Int) // Must contain all nodes possible

  case class Element(name: String,
                     `type`: String,
                     values: List[Double],
                     nodes: NodeType)

  case class Complex(real: Double, imaginary: Double)

  /**
    *   Lists
    */
  //TODO Solve compilation errors
    var netList = List[Element]
 // var netList = ListBuffer[Element]()
 // val netList = new Array[Element](maximumElements)

  val list: Array[String] = new Array[String](maximumNodes+1)

  /**
    * Internal states
    */
  var variableIterator = 0
  var elementIterator = 0
  var nodeIterator = 0

  var tryCounter: Int = 0
  var randomCounter: Int = 0

  var numerationCurrent: Int = _
  var numerationTime: Int = _
  var firstIteration: Boolean = _

  //TODO Check if really starts with null value
  val admittanceMatrix = Array.ofDim[Double](maximumNodes+1, maximumNodes+2)

  val currentSolution = new Array[Double](maximumNodes+1)
  val lastSolution = new Array[Double](maximumNodes+1)


  /**
    * Analysis parameter
    */
  var useInitialConditions: Boolean = false
  var useNewtonRaphson: Boolean = false

  var totalTime : Double = _
  var stepSize : Double = _
  var stepsPerPoint : Double = _

  /**
    * Greet message
    */
  println("EEL525 - Electric Circuits II")
  println("Developed by Matheus Hoffmann - hoffmann@poli.ufrj.br")
  println("Circuit Analysis in the time domain containing liner/nonlinear components")
  println(s"Version -> $version")
  println("Insert NetList file (example.net):")
  val fileNetList = scala.io.StdIn.readLine()
  //println()

  val listOfElements = openFile(fileNetList)

  val numberOfElements = listOfElements.size

  if (numberOfElements > maximumElements) {
    println(s"Maximum of elements reached, please enter a NetList with less than $maximumElements elements.")
    sys.exit(1)
  }

  /**
    * Execution Flow
    */
  ClearScreen()
  GetOutputFileName()
  GetNetListAndAnalysisParameters()
  println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
  println(s"This circuit has -> $nodeIterator nodes, $variableIterator variables and $elementIterator elements.\n")
  println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
  if(debug) {
    ClearScreen()
    ShowContext()
  }
  StartTimeAnalysis()
  //TODO output filename to check .tab or csv, tries and simulation total time
  println("Simulation success! Total time -> with -> tries.")
  println("Results saved at -> file")
  println("Thanks for using Scala Spice.")
  println("  ________                     .___    __________                 ._.                                  \n /  _____/   ____    ____    __| _/    \\______   \\ ___.__.  ____  | |                                  \n/   \\  ___  /  _ \\  /  _ \\  / __ |      |    |  _/<   |  |_/ __ \\ | |                                  \n\\    \\_\\  \\(  <_> )(  <_> )/ /_/ |      |    |   \\ \\___  |\\  ___/  \\|                                  \n \\______  / \\____/  \\____/ \\____ |      |______  / / ____| \\___  > __                                  \n        \\/                      \\/             \\/  \\/          \\/  \\/  ")
  sys.exit(0)
  //Plot, Interface
  /**
    * End
    */

  //TODO Test clear on Win
  def ClearScreen(): Unit = {
    if (linux) {
      val clear = "clear" !
    } else {
      val clear = "cls" !
    }
  }
  /**
    * Set Output file name from user.
    */
  //TODO Remember dot problems on Win systems
  def GetOutputFileName(): Unit = {
    println("Chose output file name, if you want same _.net name file press '1':")
    var fileOutput = scala.io.StdIn.readLine()
    if(fileOutput=="1"){
      fileOutput = fileNetList + "result"
    }
    println(s"Output file name -> === ! === $fileOutput === ! ===")

  }

  /**
    * Reading netList and converting to Element objects
    */
  def GetNetListAndAnalysisParameters(): Unit = {
    listOfElements.foreach(element => {
      elementIterator += 1 //don't use netList(0)
      element(0).toUpper match {
        case x if x == 'R' || x == 'I' || x == 'V' =>
          netList(elementIterator).`type` = element(0)
          netList(elementIterator).name += getParameters(element)(0)
          netList(elementIterator).nodes.nodeA += Number(getParameters(element)(1))
          netList(elementIterator).nodes.nodeB += Number(getParameters(element)(2))
          netList(elementIterator).values.head += getParameters(element)(3)

        case x if x == 'L' || x == 'C' =>
          netList(elementIterator).`type` += element(0)
          netList(elementIterator).name += getParameters(element)(0)
          netList(elementIterator).nodes.nodeA += Number(getParameters(element)(1))
          netList(elementIterator).nodes.nodeB += Number(getParameters(element)(2))
          netList(elementIterator).values.head += getParameters(element)(3)
          netList(elementIterator).values(1) += getParameters(element)(4) // Initial Condition

        case x if x == 'G' || x == 'E' || x == 'F' || x == 'H' =>
          netList(elementIterator).`type` += element(0)
          netList(elementIterator).name += getParameters(element)(0)
          netList(elementIterator).nodes.nodeA += Number(getParameters(element)(1))
          netList(elementIterator).nodes.nodeB += Number(getParameters(element)(2))
          netList(elementIterator).nodes.nodeC += Number(getParameters(element)(3))
          netList(elementIterator).nodes.nodeD += Number(getParameters(element)(4))
          netList(elementIterator).values.head += getParameters(element)(5)

        case 'O' =>
          netList(elementIterator).`type` += element(0)
          netList(elementIterator).name += getParameters(element)(0)
          netList(elementIterator).nodes.nodeA += Number(getParameters(element)(1))
          netList(elementIterator).nodes.nodeB += Number(getParameters(element)(2))
          netList(elementIterator).nodes.nodeC += Number(getParameters(element)(3))
          netList(elementIterator).nodes.nodeD += Number(getParameters(element)(4))

        case '$' =>
          netList(elementIterator).`type` += element(0)
          netList(elementIterator).name += getParameters(element)(0)
          netList(elementIterator).nodes.nodeA += Number(getParameters(element)(1))
          netList(elementIterator).nodes.nodeB += Number(getParameters(element)(2))
          netList(elementIterator).nodes.nodeC += Number(getParameters(element)(3))
          netList(elementIterator).nodes.nodeD += Number(getParameters(element)(4))
          netList(elementIterator).values.head += getParameters(element)(5)
          netList(elementIterator).values(1) += getParameters(element)(6)
          netList(elementIterator).values(2) += getParameters(element)(7)

        /** TODO Implement
          * case 'N' =>
          * netList(elementIterator).`type` += element(0)
          * netList(elementIterator).name += getParameters(element)(0)
          * netList(elementIterator).nodes.nodeA += Number(getParameters(element)(1))
          * netList(elementIterator).nodes.nodeB += Number(getParameters(element)(2))
          * netList(elementIterator).values.head += getParameters(element)(3)
          * netList(elementIterator).values(1) += getParameters(element)(4)
          * netList(elementIterator).values(2) += getParameters(element)(5)
          * netList(elementIterator).values(3) += getParameters(element)(6)
          */

        case 'K' =>
          netList(elementIterator).`type` += element(0)
          netList(elementIterator).name += getParameters(element)(0)
          netList(elementIterator).nodes.nodeA += Number(getParameters(element)(1))
          netList(elementIterator).nodes.nodeB += Number(getParameters(element)(2))
          netList(elementIterator).nodes.nodeC += Number(getParameters(element)(3))
          netList(elementIterator).nodes.nodeD += Number(getParameters(element)(4))
          netList(elementIterator).values.head += getParameters(element)(5)

        //TODO optional params verify
        case 'D' => // DC Source
          if (element.substring(0, 1).toUpperCase != "DC") {
            printf("Unknown element. Aborting analysis.")
            sys.exit(1)
          } else {
            netList(elementIterator).`type` += "DC"
            netList(elementIterator).values.head += getParameters(element)(1)
          }

        //TODO optional params verify
        case 'S' => //SIN Source
          if (element.substring(0, 2).toUpperCase != "SIN") {
            printf("Unknown element. Aborting analysis.")
            sys.exit(1)
          } else {
            netList(elementIterator).`type` += "SIN"
            netList(elementIterator).values.head += getParameters(element)(1)
            netList(elementIterator).values(1) += getParameters(element)(2)
            netList(elementIterator).values(2) += getParameters(element)(3)
            netList(elementIterator).values(3) += getParameters(element)(4)
            netList(elementIterator).values(4) += getParameters(element)(5)
            netList(elementIterator).values(5) += getParameters(element)(6)
            netList(elementIterator).values(6) += getParameters(element)(7)
          }

        case 'P' => // PULSE Source
          if (element.substring(0, 4).toUpperCase != "PULSE") {
            printf("Unknown element. Aborting analysis.")
            sys.exit(1)
          } else {
            netList(elementIterator).`type` += "PULSE"
            netList(elementIterator).values.head += getParameters(element)(1)
            netList(elementIterator).values(1) += getParameters(element)(2)
            netList(elementIterator).values(2) += getParameters(element)(3)
            netList(elementIterator).values(3) += getParameters(element)(4)
            netList(elementIterator).values(4) += getParameters(element)(5)
            netList(elementIterator).values(5) += getParameters(element)(6)
            netList(elementIterator).values(6) += getParameters(element)(7)
            netList(elementIterator).values(7) += getParameters(element)(8)
          }


        case '*' =>
          println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
          println(s"Comment: $element\n")
          println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
          elementIterator -= 1

        case '.' =>
          totalTime = getParameters(element)(1).toDouble
          stepSize = getParameters(element)(2).toDouble
          stepsPerPoint = getParameters(element)(4).toDouble
          elementIterator -= 1

        case _ =>
          printf("Unknown element. Aborting analysis.")
          sys.exit(1)
      }


    })

    // Add current variables in nodes
    nodeIterator = variableIterator
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
  }

  /**
    * Show everything on current state inside the application, for debug.
    */
  //TODO treat possible null, need JX and JY
  def ShowContext(): Unit = {
    var variableCounter: Int = 1
    list.foreach(variable => {
      println(s"Variable ->    $variableCounter   ===>    $variable")
      variableCounter += 1
})
    if(variableCounter != variableIterator){
      println(s"Miss matching exception: List has ${list.length} and $variableIterator size. ")
    }

    netList.foreach(element => {
      println(s"Element -> ${element.`type`} | Values -> ${element.values} | Nodes -> ${element.nodes}")
      println(s"$element") // Just test printable
    })

    println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
    println(s"This circuit has -> $nodeIterator nodes, $variableIterator variables and $elementIterator elements.\n")
    println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
  }


  /**
    * Auxiliar function that name nodes
    */
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
  /**
    * Solve system using Gaussian-Jordan method.
   */
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
      if(Math.abs(idxT) < TOLG){
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
  /**
    * Build stamps to each element
    */
  def BuildStamps(): Unit ={
    for (idx <- 1 to elementIterator){
      netList(idx).`type` match {
        case "L" =>
          //Numeric integration must deliver G and Z
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeA) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeB) += 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeX) += g
          admittanceMatrix(netList(idx).nodes.nodeX)(variableIterator+1) += z

        case "C" =>
          //Numeric integration must deliver G and Z
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeA) += g
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeB) += g
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeB) -= g
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeA) -= g
          admittanceMatrix(netList(idx).nodes.nodeA)(variableIterator+1) += z
          admittanceMatrix(netList(idx).nodes.nodeB)(variableIterator+1) -= z

        case "R" =>
          val conductance = 1/netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeA) += conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeB) += conductance
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeB) -= conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeA) -= conductance

        case "G" =>
          val conductance = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeC) += conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeD) += conductance
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeD) -= conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeC) -= conductance

        case "I" =>
          val current = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(variableIterator+1) -= current
          admittanceMatrix(netList(idx).nodes.nodeB)(variableIterator+1) += current

        case "V" =>
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeA) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeB) += 1

        case "E" =>
          val conductance = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeA) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeB) += 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeC) += conductance
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeD) -= conductance

        case "F" =>
          val conductance = netList(idx).values.head
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += conductance
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= conductance
          admittanceMatrix(netList(idx).nodes.nodeC)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeD)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeC) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeD) += 1

        case "H" =>
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

        case "$" =>

        case "O" =>
          admittanceMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX) += 1
          admittanceMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX) -= 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeC) += 1
          admittanceMatrix(netList(idx).nodes.nodeX)(netList(idx).nodes.nodeD) -= 1

        case "N" =>

        case "DC" =>
          admittanceMatrix(netList(idx).nodes.nodeX)(variableIterator+1) -= netList(idx).values.head

        case "SIN" =>

        case "PULSE" =>

        case x: String =>
          println(s"Unknown type in list -> $x.")
      }
    }
  }

  /**
    * Util functions: Parsing, FileHandler, etc
    */

  def getParameters(element: String): Array[String] = {
    element.split(" ")
  }

  def openFile(fileName: String): List[String] = {
    Source.fromFile(fileName).getLines.toList
  }

  def writeLine()={}


}

/**
  * Improvements
  *
  * Logger, Time execution details, plotter, counter, EDFIL, interface
  */