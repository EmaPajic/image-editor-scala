package image

import scala.collection.mutable.ListBuffer
import utils._

case class Layer(val idx: Int, var image: Image, val transparency: Double, var isActive: Boolean = true) {
  if (transparency < 0 || transparency > 1) throw new IllegalArgumentException
  val originalImage = utils.deepCopy(image.image)
  def activate: Unit = isActive = true
  def deactivate: Unit = isActive = false
  val operationsApplied: ListBuffer[(String, String, Double, Int, String)] = ListBuffer()
}

object Layer {
  def maxW(l1: Layer, l2: Layer): Layer = if (l1.image.width > l2.image.width) l1 else l2
  def maxH(l1: Layer, l2: Layer): Layer = if (l1.image.height > l2.image.height) l1 else l2
  def minW(l1: Layer, l2: Layer): Layer = if (l1.image.width < l2.image.width) l1 else l2
  def minH(l1: Layer, l2: Layer): Layer = if (l1.image.height < l2.image.height) l1 else l2
}