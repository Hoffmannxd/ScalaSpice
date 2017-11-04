package protocol


object Protocols {

  case class Element(name: String,
                     `type`: String,
                     values: List[Double],
                     index: List[Int])

  case class Complex(real: Double, imaginary: Double)

}



