import scala.collection.mutable.ListBuffer

val currentSolution = new Array[Double](50)

printf(s"${currentSolution(0)}")

case class HaHa(inteiro: Int, str: String)

var hahaList = ListBuffer[HaHa]()  //List(HaHa(12,"lala"),HaHa(13, "kaka"))
hahaList += HaHa(12,"lala")
hahaList += HaHa(13, "kaka")


hahaList.update(1,HaHa(99,"zaza"))

println(hahaList)

var testExp = 1E-3