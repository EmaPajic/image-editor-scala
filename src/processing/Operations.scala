package processing

import scala.collection.mutable.{HashMap, ListBuffer, ArrayBuffer}
import image.{Layer, Image, Selections}

object Operations extends Operable {
  val add: (Double, Double) => Double = (a, b) => a + b
  val sub: (Double, Double) => Double = (a, b) => a - b
  val invsub: (Double, Double) => Double = (a, b) => b - a
  val mul: (Double, Double) => Double = (a, b) => a * b
  val div: (Double, Double) => Double = (a, b) => a / b
  val invdiv: (Double, Double) => Double = (a, b) => b / a
  val pow: (Double, Double) => Double = (a, b) => scala.math.pow(a, b)
  val log: (Double, Double) => Double = (a, b) => scala.math.log(a)
  val abs: (Double, Double) => Double = (a, b) => scala.math.abs(a)
  val max: (Double, Double) => Double = (a, b) => scala.math.max(a, b)
  val min: (Double, Double) => Double = (a, b) => scala.math.min(a, b)
  val identity: (Double, Double) => Double = (a, b) => a
  val invert: (Double, Double) => Double = (a, b) => 1.0 - a
  def limit(a: Double): Double = scala.math.max(scala.math.min(a, 1.0), 0.0)
  def noLimit(a: Double): Double = a
  
  val operations: HashMap[String, (Double, Double) => Double] = HashMap(("add", add),
      ("sub", sub), ("invsub", invsub), ("mul", mul), ("div", div), ("invdiv", invdiv), ("pow", pow),
      ("log", log), ("abs", abs), ("max", max), ("min", min), ("identity", identity), ("invert", invert))
  
      
  val limiters: HashMap[String, Double => Double] = HashMap(("limit", limit), ("noLimit", noLimit))
  
  def operation(name: String, const: Double): Double => Double = {
    def f(param: Double): Double = {
      operations(name)(param, const)
    }
    f
  }
  
  def operationC(name: String, const: Double): Array[Array[Array[Double]]] => Array[Array[Array[Double]]] = {
    def f(mat: Array[Array[Array[Double]]]): Array[Array[Array[Double]]] = {
      val res = Array.ofDim[Double](mat.length, mat(0).length, 4)
      for (channel <- 0 to 2; col <- 0 until mat.length; row <- 0 until mat(0).length) {
        res(col)(row)(channel) = operation(name, const)(mat(col)(row)(channel))
      }
      res
    }
    f
  }
}