package settings


trait Settings {
  /**
    * App and environment parameters
    */
  val version: String = "0.0.2 - 05/11/2017"
  val linux: Boolean = true
  val debug: Boolean = true

  /**
    * Circuit Bounds
    */
  val maximumLines: Int = 80
  val maximumName: Int = 11
  val maximumElements: Int  = 50
  val maximumNodes: Int = 50


  /**
    * Newton-Raphson linearization with Gmin stepping
    */
  val initialVoltage: Int = 1
  val initialCurrent: Int = 1
  val maximumTries: Double = math.pow(10, 2)
  val maximumRandom: Double = math.pow(10, 2)
  val voltageTolerance: Double = math.pow(10, -9)
  val currentTolerance: Double = math.pow(10, -9)
  val conductanceAssistantStart: Int = 1
  val conductanceAssistantMinimum: Int = 1
  val conductanceAssistantMaximum: Int = 1
  val conductanceAssistantInitialMultiplier: Int = 1
  val minimumDivisor: Int = 1
  val minimumDivisorMultiplier: Int = 1

  /**
    * Math const
    */
  val pi: Double =   math.Pi
  val TOLG: Double = math.pow(10, -9)


  println("Circuit Analysis parameters loaded")
  if(linux) println("OS Linux formatting") else println("OS Microsoft Windows formatting")
  println(s"Debug option -> $debug")
  println("\n ________  ________  ________  ___       ________          ________  ________  ___  ________  _______      \n|\\   ____\\|\\   ____\\|\\   __  \\|\\  \\     |\\   __  \\        |\\   ____\\|\\   __  \\|\\  \\|\\   ____\\|\\  ___ \\     \n\\ \\  \\___|\\ \\  \\___|\\ \\  \\|\\  \\ \\  \\    \\ \\  \\|\\  \\       \\ \\  \\___|\\ \\  \\|\\  \\ \\  \\ \\  \\___|\\ \\   __/|    \n \\ \\_____  \\ \\  \\    \\ \\   __  \\ \\  \\    \\ \\   __  \\       \\ \\_____  \\ \\   ____\\ \\  \\ \\  \\    \\ \\  \\_|/__  \n  \\|____|\\  \\ \\  \\____\\ \\  \\ \\  \\ \\  \\____\\ \\  \\ \\  \\       \\|____|\\  \\ \\  \\___|\\ \\  \\ \\  \\____\\ \\  \\_|\\ \\ \n    ____\\_\\  \\ \\_______\\ \\__\\ \\__\\ \\_______\\ \\__\\ \\__\\        ____\\_\\  \\ \\__\\    \\ \\__\\ \\_______\\ \\_______\\\n   |\\_________\\|_______|\\|__|\\|__|\\|_______|\\|__|\\|__|       |\\_________\\|__|     \\|__|\\|_______|\\|_______|\n   \\|_________|                                              \\|_________|                                  \n                                                                                                           \n                                                                                                           \n")


}

object Settings extends Settings



