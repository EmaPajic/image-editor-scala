package image

import scala.collection.mutable.HashSet

case class SingleSelection(leftTop: (Int, Int) = (0, 0), rightBottom: (Int, Int) = (0, 0)) {
  val points: HashSet[(Int, Int)] = HashSet()
  for (i <- leftTop._1 to rightBottom._1; j <- leftTop._2 to rightBottom._2)
    points.add((i, j))
        
  override def toString: String = {
    leftTop._1.toString + "," + leftTop._2.toString + "," + rightBottom._1.toString + "," + rightBottom._2.toString
  }
}