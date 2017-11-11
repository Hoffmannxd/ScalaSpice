package protocol


object Protocols {

  case class NodeType(nodeA: Int, nodeB: Int, nodeC: Int, nodeD: Int, nodeX: Int, nodeY: Int) // Must contain all nodes possible

  case class Element(name: String,
                     `type`: String,
                     values: List[Double],
                     nodes: NodeType)

  case class Complex(real: Double, imaginary: Double)

}



