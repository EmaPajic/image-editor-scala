package image

import Pixel._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import processing._

case class Image(val image: BufferedImage) {
  val width = image.getWidth
  val height = image.getHeight
  val pixelMatrix = Array.ofDim[Double](width, height, 4)
  
  for (col <- 0 until width; row <- 0 until height) {
      val pix = Pixel(image.getRGB(col, row))
      pixelMatrix(col)(row)(0) = pix.r / 255.0
      pixelMatrix(col)(row)(1) = pix.g / 255.0
      pixelMatrix(col)(row)(2) = pix.b / 255.0
      pixelMatrix(col)(row)(3) = pix.a / 255.0
  }
  
  def getPixel(col: Int, row: Int): Pixel = {
    val r = (Operations.limit(pixelMatrix(col)(row)(0)) * 255).toInt
    val g = (Operations.limit(pixelMatrix(col)(row)(1)) * 255).toInt
    val b = (Operations.limit(pixelMatrix(col)(row)(2)) * 255).toInt
    val a = (Operations.limit(pixelMatrix(col)(row)(3)) * 255).toInt
    Pixel(r, g, b, a)
  }
	
  def toBufferedImage: BufferedImage = {
    val bufferedImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for (col <- 0 until width; row <- 0 until height) 
        bufferedImg.setRGB(col, row, getPixel(col, row).toInt)
    bufferedImg
  }
  
  def toBufferedImageJPG: BufferedImage = {
    val bufferedImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (col <- 0 until width; row <- 0 until height) 
        bufferedImg.setRGB(col, row, getPixel(col, row).toInt)
    bufferedImg
  }
  
  def isOnImage(point: (Int, Int)): Boolean = {
    point._1 >= 0 && point._1 < width && point._2 >= 0 && point._2 < height
  }
  
  def applyOperation(f: Double => Double, limit: Double => Double, selected: Selection): Unit = {
    for (point <- selected.points; if isOnImage(point)) {
      for (channel <- 0 to 2) {
        pixelMatrix(point._1)(point._2)(channel) = limit(f(pixelMatrix(point._1)(point._2)(channel)))
      }
    }
  }

  def applyFilter(filter: Array[Array[Array[Double]]] => Array[Array[Array[Double]]], selected: Selection): Unit = {
    val newPixelMatrix = filter(pixelMatrix)
    for (point <- selected.points; if isOnImage(point)) {
      for (channel <- 0 to 2) {
        pixelMatrix(point._1)(point._2)(channel) = newPixelMatrix(point._1)(point._2)(channel)
      }
    }
  }
  
  def applyOperationOrFilter(name: String, selected: Selection, const: Double = 0.0, lim: Double => Double = Operations.noLimit): Unit = {
    if (Filters.filters.contains(name)) {
      applyFilter(Filters.filter(name), selected)
    }
    else if (Operations.operations.contains(name)){
      applyOperation(Operations.operation(name, const), lim, selected)
    }
  }
}

object Image extends App {
  def apply(pixels: ArrayBuffer[ArrayBuffer[Pixel]], width: Int, height: Int): Image = {
    val temp = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for (col <- 0 until width; row <- 0 until height)
        temp.setRGB(col, row, pixels(col)(row).toInt)
    Image(temp)
  }
  
  def apply(path: String):Image = {
    Image(ImageIO.read(new File(path)))
  }
  
  def apply(): Image = {
    val temp = new BufferedImage(640, 480, BufferedImage.TYPE_INT_ARGB)
    for (col <- 0 until 640; row <- 0 until 480)
        temp.setRGB(col, row, Pixel().toInt)
    Image(temp)
  }

  def formatFromPath(path: String): String = {
    val jpg: Regex = "\\.jpg".r
    val it = jpg.findAllIn(path)
    if (it.hasNext) "jpg" else "png"
  }
  
  def save(image: Image, file: java.io.File) = {
    if (formatFromPath(file.toString()) == "jpg") ImageIO.write(image.toBufferedImageJPG, "jpg", file)
    else ImageIO.write(image.toBufferedImage, "png", file)
  }
}