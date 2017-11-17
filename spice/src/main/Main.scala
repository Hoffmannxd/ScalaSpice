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
import scala.util.Try

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
  case class NodeType(nodeA: Int,
                      nodeB: Int,
                      nodeC: Option[Int],
                      nodeD: Option[Int],
                      nodeX:  Option[Int],
                      nodeY: Option[Int]) // Must contain all nodes possible

  case class Element(name: String,
                     `type`: String,
                     values: List[Double],
                     nodes: NodeType)

  case class Complex(real: Double, imaginary: Double)

  /**
    *   Lists
    */
  //TODO Solve compilation errors

  var netList = ListBuffer[Element]()

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

  var firstIteration: Boolean = true
  var startTime: Boolean = true

  var convergence: Boolean = false
  var stepIterator: Int = 0

  /**
    * Matrix that store information about nodes, representing Modified Nodal Analysis
    */
  val systemMatrix = Array.ofDim[Double](maximumNodes+1, maximumNodes+2)
  val currentSolution = new Array[Double](maximumNodes+1)
  val lastSolution = new Array[Double](maximumNodes+1)


  /**
    * Analysis parameter
    */
  // Obs these parameters are for debugging seeing unit testing
  var useInitialConditions: Boolean = false
  var useNewtonRaphson: Boolean = false
  var useGMinStepInitializationMethod: Boolean = false

  // TODO Make sure notation XE-Y works, if need create a parser/regex
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
  //Making a builder makes much less verbose
  def GetNetListAndAnalysisParameters(): Unit = {
    listOfElements.foreach(element => {
      elementIterator += 1 //don't use netList(0)
      element(0).toUpper match {
        case x if x == 'R' =>
          val `type`  = element(0).toString
          val name = getParameters(element)(0)
          val values : List[Double] = List(getParameters(element)(3).toDouble)
          val nodes = NodeType(Number(getParameters(element)(1)),Number(getParameters(element)(2)), None, None, None, None)
          netList += Element(name,`type`, values, nodes)

        case x if x == 'I' || x == 'V' =>{
          val name = getParameters(element)(0)
          val nodes = NodeType(Number(getParameters(element)(1)),Number(getParameters(element)(2)), None, None, None, None)
          if(isDoubleNumber(getParameters(element)(3))){
            val `type` = element(0).toString
            val values: List[Double] = List(getParameters(element)(3).toDouble)
            netList += Element(name,`type`, values, nodes)
          } else {
          getParameters(element)(3).toUpperCase match {

            //TODO optional params verify
            case "DC" =>
              val `type` = getParameters(element)(3).toUpperCase
              val values: List[Double] = List(getParameters(element)(4).toDouble)
              netList += Element(name,`type`, values, nodes)


            //TODO optional params verify
            case "SIN" =>
              val `type` = getParameters(element)(3).toUpperCase
              val values: List[Double] = List(getParameters(element)(4).toDouble,
                getParameters(element)(5).toDouble,
                getParameters(element)(6).toDouble,
                getParameters(element)(7).toDouble,
                getParameters(element)(8).toDouble,
                getParameters(element)(9).toDouble,
                getParameters(element)(10).toDouble
              )
              netList += Element(name,`type`, values, nodes)


            case "PULSE" =>
              val `type` = getParameters(element)(3).toUpperCase
              val values: List[Double] = List(getParameters(element)(4).toDouble,
                getParameters(element)(5).toDouble,
                getParameters(element)(6).toDouble,
                getParameters(element)(7).toDouble,
                getParameters(element)(8).toDouble,
                getParameters(element)(9).toDouble,
                getParameters(element)(10).toDouble,
                getParameters(element)(10).toDouble
              )
              netList += Element(name,`type`, values, nodes)

            case _ =>

          }
          }
        }

        case x if x == 'L' || x == 'C' =>
          val `type`  = element(0).toString
          val name = getParameters(element)(0)
          val values : List[Double] = List(getParameters(element)(3).toDouble,getParameters(element)(4).toDouble) //<- IC
          val nodes = NodeType(Number(getParameters(element)(1)),Number(getParameters(element)(2)), None, None, None, None)
          netList += Element(name,`type`, values, nodes)

        case x if x == 'G' || x == 'E' || x == 'F' || x == 'H' =>
          val `type`  = element(0).toString
          val name = getParameters(element)(0)
          val values : List[Double] = List(getParameters(element)(5).toDouble)
          val nodes = NodeType(Number(getParameters(element)(1)),Number(getParameters(element)(2)), Some(Number(getParameters(element)(3))), Some(Number(getParameters(element)(4))), None, None)
          netList += Element(name,`type`, values, nodes)

        case 'O' =>
          val `type`  = element(0).toString
          val name = getParameters(element)(0)
          val nodes = NodeType(Number(getParameters(element)(1)),Number(getParameters(element)(2)), Some(Number(getParameters(element)(3))), Some(Number(getParameters(element)(4))), None, None)
          netList += Element(name,`type`, null, nodes)

        case '$' =>
          val `type`  = element(0).toString
          val name = getParameters(element)(0)
          val values : List[Double] = List(getParameters(element)(5).toDouble,getParameters(element)(6).toDouble,getParameters(element)(7).toDouble)
          val nodes = NodeType(Number(getParameters(element)(1)),Number(getParameters(element)(2)), Some(Number(getParameters(element)(3))), Some(Number(getParameters(element)(4))), None, None)
          netList += Element(name,`type`, values, nodes)

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
          val `type`  = element(0).toString
          val name = getParameters(element)(0)
          val values : List[Double] = List(getParameters(element)(5).toDouble)
          val nodes = NodeType(Number(getParameters(element)(1)),Number(getParameters(element)(2)), Some(Number(getParameters(element)(3))), Some(Number(getParameters(element)(4))), None, None)
          netList += Element(name,`type`, values, nodes)

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
        case x if x == 'V' || x == 'E' || x == 'F' || x == 'O' => //TODO Add DC PULSE SIN here?
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
          list(variableIterator) = "jy" + netList(idx).name
          netList.update(idx,Element(netList(idx).name,
            netList(idx).`type`,
            netList(idx).values,
            NodeType(netList(idx).nodes.nodeA,
              netList(idx).nodes.nodeB,
              netList(idx).nodes.nodeC,
              netList(idx).nodes.nodeD,
              Some(variableIterator - 1),
              Some(variableIterator))))
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
      println(s"Element -> ${element.`type`} | Name -> ${element.name}| Values -> ${element.values} | Nodes -> ${element.nodes}")
      println(s"$element") // Just test printable
    })

    println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
    println(s"This circuit has -> $nodeIterator nodes, $variableIterator variables and $elementIterator elements.\n")
    println("°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸°º¤ø,¸¸,ø¤º°`°º¤ø,¸,ø¤°º¤ø,¸¸,ø¤º°`°º¤ø,¸\n")
  }

  def StartTimeAnalysis(): Unit = {
    firstIteration = true

    //TODO check if this fraction is int
    for (idxOne <- 0 to (totalTime/stepSize).toInt){

      val internalStep: Int = 0

      while(internalStep<stepsPerPoint){
        while(!convergence){
          if(tryCounter >= maximumTries && randomCounter < maximumRandom){
            for(idxTwo <- 1 to variableIterator) lastSolution(idxTwo) = math.random
            randomCounter += 1
            tryCounter = 0
          }
          if(randomCounter >= maximumRandom){
            println("Simulator reached maximum randomizations, any valid solution found, please change circuit or analysis parameters")
            sys.exit(1)
          }
          BuildStamps()
          Solve()
          if(useNewtonRaphson){
            for(idxThree <- 1 to variableIterator){
              math.abs(lastSolution(idxThree)   -   systemMatrix(idxThree)(variableIterator+1)) match {
                case error if error > errorTolerance =>
                  tryCounter += 1
                case error =>
                  convergence = true
              }

            }
          }

          /**
            * Endpoint of iteraction, retry: update Last Solution.
            */
          for(idxFour <- 1 to variableIterator){
            lastSolution(idxFour) = systemMatrix(idxFour)(variableIterator+1)
            currentSolution(idxFour) = systemMatrix(idxFour)(variableIterator+1)
          }
          // lastSolution =

          //TODO writeRegister checking state === ! === increment stepIterator and time += step/stepSize
          //InitializeOutputFile()
          //WriteRegister()



        }

        startTime = false
        //Already passed throught

      }
    }

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
        if(Math.abs(systemMatrix(idxL)(idxI)) > Math.abs(idxT)){
          idxA = idxL
          idxT = systemMatrix(idxL)(idxI)
        }
      }
      if(idxI != idxA){
        for(idxL <- 1 to variableIterator){
          idxP = systemMatrix(idxI)(idxL)
          systemMatrix(idxI)(idxL) = systemMatrix(idxA)(idxL)
          systemMatrix(idxA)(idxL) = idxP
        }
      }
      if(Math.abs(idxT) < TOLG){
        println("Singular system")
        sys.exit(1)
      }
      for(idxJ <- variableIterator+1 to (0, -1)){
        systemMatrix(idxI)(idxJ) /= idxT //TODO Check
        idxP=systemMatrix(idxI)(idxJ)
        if(idxP != 0){
          for (idxL <- 1 to variableIterator){
            if(idxL != idxI){
              systemMatrix(idxL)(idxI) -= systemMatrix(idxL)(idxI)*idxP
            }
          }
        }
      }
    }
  }


  /**
    * Build stamps to each element
    */
  def BuildStamps(): Unit ={
    for (idx <- 1 to elementIterator){
      netList(idx).`type` match {
        case "L" =>
          /**
            * Trapezoidal rule for inductor
            */
          val conductance = if (startTime){
            netList(idx).values.head/(stepSize/minimumDivisor)
          } else {
            netList(idx).values.head/(0.5*(stepSize/stepIterator)/netList(idx).values.head)
          }
            val voltage = if(startTime){
              if(useInitialConditions) return netList(idx).values(1)/(stepSize/stepIterator)
              return 0
            } else {
              if(firstIteration){
                netList.update(idx, Element(netList(idx).name,
                  netList(idx).`type`,
                  List(netList(idx).values.head,
                    currentSolution(netList(idx).nodes.nodeX.get), // Updating initial condition
                    currentSolution(netList(idx).nodes.nodeX.get),
                    currentSolution(netList(idx).nodes.nodeA) - currentSolution(netList(idx).nodes.nodeB)),
                netList(idx).nodes))

                netList(idx).values(2)

              } else { // TODO possible null exception
                (2*netList(idx).values.head)/(stepSize/stepIterator)*netList(idx).values(1)
                + netList(idx).values(2)
              }
            }
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX.get) = 1
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX.get) = -1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeA) = 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeB) = -1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeX.get) += conductance
          systemMatrix(netList(idx).nodes.nodeX.get)(variableIterator+1) -= voltage

        case "C" =>
          /**
            * Trapezoidal rule for capacitor
            */
          val conductance = if (startTime){
            (stepSize/minimumDivisor)/netList(idx).values.head
          } else {
            0.5*(stepSize/stepIterator)/netList(idx).values.head
          }
          val current = if(startTime){
            if(useInitialConditions) return netList(idx).values(1)
            return 0
          } else {
            if(firstIteration){
              netList.update(idx, Element(netList(idx).name,
                netList(idx).`type`,
                List(netList(idx).values.head,
                  currentSolution(netList(idx).nodes.nodeA) - currentSolution(netList(idx).nodes.nodeB),
                  currentSolution(netList(idx).nodes.nodeA) - currentSolution(netList(idx).nodes.nodeB)),
                netList(idx).nodes))

                netList(idx).values(1)
            } else {
              currentSolution(netList(idx).nodes.nodeX.get)*(stepSize/stepIterator)/(2*netList(idx).values.head)
              + netList(idx).values(1)
            }
          }
          // Check with 2016.1 if stamps are correctly here
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX.get) = 1
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX.get) = -1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeA) = 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeB) = -1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeX.get) -= conductance
          systemMatrix(netList(idx).nodes.nodeX.get)(variableIterator+1) += current

        case "R" =>
          val conductance = 1/netList(idx).values.head
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeA) += conductance
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeB) += conductance
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeB) -= conductance
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeA) -= conductance

        case "G" =>
          val conductance = netList(idx).values.head
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeC.get) += conductance
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeD.get) += conductance
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeD.get) -= conductance
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeC.get) -= conductance

        case "I" =>
          val current = netList(idx).values.head
          systemMatrix(netList(idx).nodes.nodeA)(variableIterator+1) -= current
          systemMatrix(netList(idx).nodes.nodeB)(variableIterator+1) += current

        case "V" =>
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX.get) += 1
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX.get) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeA) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeB) += 1

        case "E" =>
          val conductance = netList(idx).values.head
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX.get) += 1
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX.get) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeA) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeB) += 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeC.get) += conductance
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeD.get) -= conductance

        case "F" =>
          val conductance = netList(idx).values.head
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX.get) += conductance
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX.get) -= conductance
          systemMatrix(netList(idx).nodes.nodeC.get)(netList(idx).nodes.nodeX.get) += 1
          systemMatrix(netList(idx).nodes.nodeD.get)(netList(idx).nodes.nodeX.get) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeC.get) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeD.get) += 1

        case "H" =>
          val conductance = netList(idx).values.head
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeY.get) += 1
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeY.get) -= 1
          systemMatrix(netList(idx).nodes.nodeC.get)(netList(idx).nodes.nodeX.get) += 1
          systemMatrix(netList(idx).nodes.nodeD.get)(netList(idx).nodes.nodeX.get) -= 1
          systemMatrix(netList(idx).nodes.nodeY.get)(netList(idx).nodes.nodeA) -= 1
          systemMatrix(netList(idx).nodes.nodeY.get)(netList(idx).nodes.nodeB) += 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeC.get) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeD.get) += 1
          systemMatrix(netList(idx).nodes.nodeY.get)(netList(idx).nodes.nodeX.get) += conductance

        case "$" =>

        case "O" =>
          systemMatrix(netList(idx).nodes.nodeA)(netList(idx).nodes.nodeX.get) += 1
          systemMatrix(netList(idx).nodes.nodeB)(netList(idx).nodes.nodeX.get) -= 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeC.get) += 1
          systemMatrix(netList(idx).nodes.nodeX.get)(netList(idx).nodes.nodeD.get) -= 1

        case "N" =>

        case "DC" =>
          systemMatrix(netList(idx).nodes.nodeX.get)(variableIterator+1) -= netList(idx).values.head

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

  def isDoubleNumber(str: String): Boolean = Try(str.toDouble).isSuccess


}

/**
  * Improvements
  *
  * Logger, Time execution details, plotter, counter, EDFIL, interface, JNI
  */