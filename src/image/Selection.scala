package image

import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap, ListBuffer}

case class Selection(name: String, val rectangles: ListBuffer[SingleSelection]) {
  val points: HashSet[(Int, Int)] = HashSet()
  var isActive: Boolean = false
  
  for (s <-rectangles) points ++= s.points
  
  def activate(): Unit = isActive = true
  def deactivate(): Unit = isActive = false
  
  override def toString: String = {
    name + " " + rectangles.toString()
  }
}