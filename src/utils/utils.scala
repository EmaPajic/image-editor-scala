package utils

import javax.imageio.ImageIO
import java.awt.image.{BufferedImage, ColorModel, WritableRaster}
import scala.io.Source
import java.io.{File, FileWriter}
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashMap}
import image.{Image, Layer, SingleSelection, Selections, BlendedImage}
import java.awt.Graphics2D
import java.awt.RenderingHints

object utils {
  def isNumeric(str: String): Boolean = str.matches("[-+]?\\d+(\\.\\d+)?")
  
  def resizeBufferedImage(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    val resizedImage: BufferedImage = new BufferedImage(width, height, image.getType())
		val graphics: Graphics2D = resizedImage.createGraphics()
		graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
		graphics.drawImage(image, 0, 0, width, height, 0, 0, image.getWidth(), image.getHeight(), null)
		graphics.dispose()
		resizedImage
	}
  
   def deepCopy(bi: BufferedImage): BufferedImage = {
     val cm: ColorModel = bi.getColorModel();
     val isAlphaPremultiplied = cm.isAlphaPremultiplied();
     val raster: WritableRaster = bi.copyData(bi.getRaster().createCompatibleWritableRaster());
     new BufferedImage(cm, raster, isAlphaPremultiplied, null);
  }
   
  def getMatrixFromOperationsString(str: String, dist: Int): Array[Array[Double]] = {
    val dim = 2 * dist + 1
    val elements = str.split('.')
    
    if (elements.size != dim * dim) throw new IllegalArgumentException
    
    val res = Array.ofDim[Double](dim, dim)
    
    for (col <- 0 until dim; row <- 0 until dim) {
      res(col)(row) = elements(row * dim + col).toDouble
    }
    
    res
  }
  
  def getOperationsStringFromMatrix(mat: Array[Array[Double]]): String = {
    val res: StringBuilder = new StringBuilder("")
    
    for (row <- 0 until mat(0).length; col <- 0 until mat.length) {
      res.append(mat(col)(row).toString)
      if (row != mat(0).length - 1 && col != mat.length - 1)
         res.append("|")
    }
    
    res.toString()
  }
  
  def readInput(file: java.io.File) = {
    val images = ListBuffer[Image]()
    val imagePaths = ListBuffer[String]()
    val transparencies = ListBuffer[Double]()
    val selectionsTemp = HashMap[String, ListBuffer[SingleSelection]]()
    val operationsTemp = HashMap[Int, ListBuffer[(String, String, Double, Int, String)]]()
    for (line <- Source.fromFile(file).getLines()) {
      val PatternImg = "(img,)(.*)".r
      val PatternTransparency = "(t,)(.*)".r
      val PatternSelections = "(s,)(.*)".r
      val PatternOperations = "(o,)(.*)".r
      line match {
        case PatternImg(img,param) => {
          images += Image(param)
          imagePaths += param
        }
        case PatternTransparency(t,param) => {
          transparencies += param.toDouble
        }
        case PatternSelections(s,param) => {
          val params = param.split(",")
          if (!selectionsTemp.contains(params(0))) {
            selectionsTemp += ((params(0), ListBuffer[SingleSelection]()))
          }
          selectionsTemp(params(0)) += SingleSelection((params(1).toInt, params(2).toInt), (params(3).toInt, params(4).toInt))
        }
        case PatternOperations(o,param) => {
          val params = param.split(",")
          if (!operationsTemp.contains(params(3).toInt)) {
            operationsTemp += ((params(3).toInt, ListBuffer[(String, String, Double, Int, String)]()))
          }
          operationsTemp(params(3).toInt) += ((params(0), params(1), params(2).toDouble, params(3).toInt, params(4)))
        }
        case _ => println("Invalid line format: " + line)
      }
    }
      
    val layersOutput = imagePaths.zip(transparencies)
    val layersTemp: ArrayBuffer[Layer] = ArrayBuffer()
    for (i <- 0 until images.size) {
      layersTemp += Layer(i, images(i), transparencies(i))
    }
    
    BlendedImage(layersTemp)
    BlendedImage(layersOutput)
        
    for (key <- selectionsTemp.keys) {
      Selections(key, selectionsTemp(key))
    }
    
    for (key <- operationsTemp.keys) {
      BlendedImage.addOperations(operationsTemp(key))
    }
 }
  
  def saveAs(file: java.io.File) {
    val layers_unzipped = BlendedImage.layersOutput.unzip
    val fileWriter = new FileWriter(file)
    
    for (image <- layers_unzipped._1) {
      fileWriter.write("img," + image + "\n")
    }
    
    for (transparency <- layers_unzipped._2) {
      fileWriter.write("t," + transparency.toString + "\n")
    }
    
    val names: ListBuffer[String] = ListBuffer()
    val singleSelections: ListBuffer[SingleSelection] = ListBuffer()
    
    for (key <- Selections.selections.keys) {
      for (singleSel <- Selections.selections(key).rectangles) {
        names += key
        singleSelections += singleSel
      }
    }
    
    for (i <- 0 until names.size; if (names(i) != "whole_image")) {
       fileWriter.write("s," + names(i) + "," + singleSelections(i).toString + "\n")
    }
    
    
    for (operation <- BlendedImage.allOperations) {
      fileWriter.write("o,"+ operation._1 + "," + operation._2 + "," + operation._3.toString + "," + operation._4.toString + "," + operation._5 + "\n")
    }
    fileWriter.close()
  }
}