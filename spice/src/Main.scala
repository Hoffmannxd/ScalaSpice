import settings.Settings.version
import sys.process._
import java.io.IOException


object Main extends App{

//TODO Solve clear screen problem
  //System.out.print("\033[H\033[2J")
  //object Cls extends App {print("\033[2J")}
  //val clear = "clear" !
  //val clear = "cls" !

  printf("\nEEL525 - Electric Circuits II\n")
  printf("by Matheus Hoffmann Fernandes Santos - hoffmann@poli.ufrj.br\n")
  printf("Circuit Analysis in the time domain containing liner/nonlinear components\n")
  printf(s"Version -> $version\n")
  printf("Insert NetList file: example.net\n")

}
