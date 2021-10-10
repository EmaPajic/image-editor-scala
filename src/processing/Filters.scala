package processing

import scala.annotation.tailrec
import scala.collection.mutable.{HashMap, ListBuffer, ArrayBuffer}
import image.{RGBAColor}

object Filters extends Operable {
  val filters: Set[String] = Set("fill", "grayscale", "median", "wam")
  var color: RGBAColor = RGBAColor.transparent
  var dist: Int = 0
  var weights: Array[Array[Double]] = Array.ofDim[Double](1, 1)
      
  def fill(mat: Array[Array[Array[Double]]], color: RGBAColor): Array[Array[Array[Double]]] = {
    val res = Array.ofDim[Double](mat.length, mat(0).length, 4)
    for (col <- 0 until mat.length; row <- 0 until mat(0).length) {
      res(col)(row)(0) = color.r.toDouble / 255.0
      res(col)(row)(1) = color.g.toDouble / 255.0
      res(col)(row)(2) = color.b.toDouble / 255.0
    }
    res
  }
  
  def grayscale(mat: Array[Array[Array[Double]]]): Array[Array[Array[Double]]] = {
    val res = Array.ofDim[Double](mat.length, mat(0).length, 4)
    for (channel <- 0 until 3; col <- 0 until mat.length; row <- 0 until mat(0).length) {
      res(col)(row)(channel) = (mat(col)(row)(0) + mat(col)(row)(1) + mat(col)(row)(2)) / 3
    }
    res
  }
  
  def arrayMedian(seq: Seq[Double]): Double = {
    val sortedSeq = seq.sortWith(_ < _)

    if (seq.size % 2 == 0) {
      val (up, down) = sortedSeq.splitAt(seq.size / 2)
      (up.last + down.head) / 2
    }
    else 
      sortedSeq(sortedSeq.size / 2)
  }
  
  def median(mat: Array[Array[Array[Double]]], dist: Int): Array[Array[Array[Double]]] = {
    val res = Array.ofDim[Double](mat.length, mat(0).length, 4)
    for (channel <- 0 until 4; col <- 0 until mat.length; row <- 0 until mat(0).length) {
      val temp: ArrayBuffer[Double] = ArrayBuffer()
      for (i <- scala.math.max(0, col - dist) to scala.math.min(mat.length - 1, col + dist);
           j <- scala.math.max(0, row - dist) to scala.math.min(mat(0).length - 1, row + dist)) {
        temp += mat(i)(j)(channel)
      }
      res(col)(row)(channel) = arrayMedian(temp)
    }
    res
  }
  
  def wam(mat: Array[Array[Array[Double]]], dist: Int, weights: Array[Array[Double]]): Array[Array[Array[Double]]] = {
    val res = Array.ofDim[Double](mat.length, mat(0).length, 4)
    for (channel <- 0 until 4; col <- 0 until mat.length; row <- 0 until mat(0).length) {
      val ilow = scala.math.max(0, col - dist)
      val ihigh = scala.math.min(mat.length - 1, col + dist)
      val jlow = scala.math.max(0, row - dist)
      val jhigh = scala.math.min(mat(0).length - 1, row + dist)
      val idim = ihigh - ilow + 1
      val jdim = jhigh - jlow + 1
       
      @tailrec
      def sumTail(idx: Int, sum: Double = 0.0): Double = {
        val i = ilow + idx % idim
        val j = jlow + idx / idim
        if (idx >= idim * jdim) sum
        else sumTail(idx + 1, sum + mat(i)(j)(channel) * weights(i - col + dist)(j - row + dist))
      }
      
      val sumcnt = sumTail(0, 0.0)
      
      res(col)(row)(channel) = 1.0 * sumcnt / idim / jdim
    }
    res
  }
  
  def filter(name: String): Array[Array[Array[Double]]] => Array[Array[Array[Double]]] = {
    def f(mat: Array[Array[Array[Double]]]): Array[Array[Array[Double]]] = {
      name match {
        case "fill" => fill(mat, color)
        case "grayscale" => grayscale(mat)
        case "median" => median(mat, dist)
        case "wam" => wam(mat, dist, weights)
      }
    }
    f
  }
}