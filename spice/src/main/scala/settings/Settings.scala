package settings


trait Settings {

  val version: String = "0.0.1 - 04/11/2017"

  val maximumLines: Int = 80
  val maximumName: Int = 11
  val maximumElements: Int  = 50
  val maximumNodes: Int = 50
  val tolg: Double = math.pow(10, -9)

  val initialVoltage: Double = 1
  val initialCurrent: Double = 1
  val maximumIterations: Double = math.pow(10, 4)
  val voltageTolerance: Double = math.pow(10, -9)
  val currentTolerance: Double = math.pow(10, -9)
  val conductanceAssistantStart: Int = 1
  val conductanceAssistantMinimum: Int = 1
  val conductanceAssistantMaximum: Int = 1
  val conductanceAssistantInitialMultiplier: Int = 1
  val minimumDivisor: Int = 1
  val minimumDivisorMultiplier: Int = 1


  val debug: Boolean = false

  printf("Circuit Analysis parameters loaded\n")
  printf(s"Debug option -> $debug\n")
  printf("\n ________  ________  ________  ___       ________          ________  ________  ___  ________  _______      \n|\\   ____\\|\\   ____\\|\\   __  \\|\\  \\     |\\   __  \\        |\\   ____\\|\\   __  \\|\\  \\|\\   ____\\|\\  ___ \\     \n\\ \\  \\___|\\ \\  \\___|\\ \\  \\|\\  \\ \\  \\    \\ \\  \\|\\  \\       \\ \\  \\___|\\ \\  \\|\\  \\ \\  \\ \\  \\___|\\ \\   __/|    \n \\ \\_____  \\ \\  \\    \\ \\   __  \\ \\  \\    \\ \\   __  \\       \\ \\_____  \\ \\   ____\\ \\  \\ \\  \\    \\ \\  \\_|/__  \n  \\|____|\\  \\ \\  \\____\\ \\  \\ \\  \\ \\  \\____\\ \\  \\ \\  \\       \\|____|\\  \\ \\  \\___|\\ \\  \\ \\  \\____\\ \\  \\_|\\ \\ \n    ____\\_\\  \\ \\_______\\ \\__\\ \\__\\ \\_______\\ \\__\\ \\__\\        ____\\_\\  \\ \\__\\    \\ \\__\\ \\_______\\ \\_______\\\n   |\\_________\\|_______|\\|__|\\|__|\\|_______|\\|__|\\|__|       |\\_________\\|__|     \\|__|\\|_______|\\|_______|\n   \\|_________|                                              \\|_________|                                  \n                                                                                                           \n                                                                                                           \n")


}

object Settings extends Settings



