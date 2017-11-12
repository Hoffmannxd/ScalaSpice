package core

import main.Main.{netList, stepSize}

class TrapezoidalRule {

def Trapezoidal(index: Int, memory: Int): Tuple2[Double, Double] = {
  var z: Double = 0.0
  var g: Double = 0.0
  if(memory == 3){// First Analysis
    z = netList(index).values(2)*(netList(index).values.head/stepSize)
    g = netList(index).values.head/stepSize
  } else {
    z =
      g =
  }
}

}

object TrapezoidalRule extends TrapezoidalRule
