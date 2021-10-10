package image

import java.awt.image.BufferedImage
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import utils._
import processing.{Operations, Filters, Operable}
import math.{min, max}
import javax.imageio.ImageIO
import java.io.File

object BlendedImage {
  val layers: ArrayBuffer[Layer] = ArrayBuffer()
  val layersOutput: ListBuffer[(String, Double)] = ListBuffer()
  val allOperations: ListBuffer[(String, String, Double, Int, String)] = ListBuffer()
  
  def apply(layersInput: ArrayBuffer[Layer]) {
    val width = min(getWidth(layersInput), 640)
    val height = min(getHeight(layersInput), 480)
    for (i <- 0 until layersInput.size) {
      val layer = layersInput(i)
  		layers += Layer(i, Image(utils.resizeBufferedImage(layer.image.toBufferedImage, width, height)), layer.transparency)
  	}
  }
  
  def toValidPoint(point: (Int, Int)): (Int, Int) = {
		((min(max(point._1, 0), getWidth(layers) - 1), min(max(point._2, 0), getHeight(layers) - 1)))
  }
  
  def apply(layersToOutput: ListBuffer[(String, Double)]) {
    layersOutput ++= layersToOutput
  }
  
  def deactivateAllLayers() {
    for (layer <- layers) layer.deactivate
  }
  
  def addOperation(op: (String, String, Double, Int, String)) {
    allOperations += op
    layers(op._4).operationsApplied += op
  }
  
  def addOperations(ops: ListBuffer[(String, String, Double, Int, String)]) {
    allOperations ++= ops
    for (op <- ops) {
      layers(op._4).operationsApplied += op
    }
  }
  
  def getWidth(layersInput: ArrayBuffer[Layer]): Int = {
    layersInput.reduce((x, y) => Layer.maxW(x, y)).image.width
  }
  
  def getHeight(layersInput: ArrayBuffer[Layer]): Int = {
    layersInput.reduce((x, y) => Layer.maxH(x, y)).image.height
  }
  
  def activateLayer(layerIdx: Int) {
    require(layerIdx < layers.size, "Invalid layer index")
    layers(layerIdx).activate
  }
  
  def deactivateLayer(layerIdx: Int) {
    require(layerIdx < layers.size, "Invalid layer index")
    layers(layerIdx).deactivate
  }
  
  def blend(): Image = {
    if (layers.find(layer => layer.isActive).isEmpty) Image()
    else {
      val width = getWidth(layers)
      val height = getHeight(layers)
      val blendPixelArray = ArrayBuffer.fill[Pixel](width, height)(Pixel())
    
      for (i <- (layers.size -1) to 0 by -1) {
        if (layers(i).isActive) {
          for (col <- 0 until layers(i).image.width; row <- 0 until layers(i).image.height)
              blendPixelArray(col)(row) = blendPixelArray(col)(row) * (1 - layers(i).transparency) + layers(i).image.getPixel(col, row) * layers(i).transparency
        }
      }
      
      Image(blendPixelArray, width, height)
    }
  }
  
  def exportBlend(file: java.io.File) {
    Image.save(blend, file)
  }
  
  def applyOperation(opName: String, const: Double) {
    for (layer <- layers if layer.isActive) {
      if (Operations.operations.contains(opName)) {
        layer.image.applyOperation(Operations.operation(opName, const), Operations.limit, Operations.getSelection())
      }
      layer.operationsApplied += ((opName, Operations.getSelection().name, const, layer.idx, "limit"))
      allOperations += ((opName, Operations.getSelection().name, const, layer.idx, "limit"))
    } 
  }
  
  def applyFilter(opName: String) {
    for (layer <- layers if layer.isActive) {
      if (Filters.filters.contains(opName)) {
        layer.image.applyFilter(Filters.filter(opName), Operations.getSelection())
      }
      if (opName == "fill") {
        layer.operationsApplied += ((opName, Operations.getSelection().name, Filters.color.toInt, layer.idx, "limit"))
        allOperations += ((opName, Operations.getSelection().name, Filters.color.toInt, layer.idx, "limit"))
      }
      else if (opName == "grayscale") {
        layer.operationsApplied += ((opName, Operations.getSelection().name, 0.0, layer.idx, "limit"))
        allOperations += ((opName, Operations.getSelection().name, 0.0, layer.idx, "limit"))
      }
      else if (opName == "median") {
        layer.operationsApplied += ((opName, Operations.getSelection().name, Filters.dist, layer.idx, "limit"))
        allOperations += ((opName, Operations.getSelection().name, Filters.dist, layer.idx, "limit"))
      }
      else if (opName == "wam") {
        val opsString = utils.getOperationsStringFromMatrix(Filters.weights)
        layer.operationsApplied += ((opName, Operations.getSelection().name, Filters.dist, layer.idx, opsString))
        allOperations += ((opName, Operations.getSelection().name, Filters.dist, layer.idx, opsString))
      }
    } 
  }
  
  def resetImages() {
    for (layer <- layers) {
      layer.image = Image(utils.resizeBufferedImage(ImageIO.read(new File(layersOutput(layer.idx)._1)), 640, 480))
    }
  }
  
  def deleteSelectionOps(selectionName: String) {
    if (selectionName != "whole_image") {
      for (layer <- layers) {
        val toDelete: ListBuffer[(String, String, Double, Int, String)] = ListBuffer()
        
        for (op <- layer.operationsApplied) {
          if (op._2 == selectionName) {
            toDelete += op
          }
        }
        layer.operationsApplied --= toDelete
        allOperations --= toDelete
      }
    }
  }
  
  def reapplyOps() {
    for (layer <- layers) {
      for (op <- layer.operationsApplied) {
        if (Operations.operations.contains(op._1) && layer.idx == op._4) {
          layer.image.applyOperation(Operations.operation(op._1, op._3), Operations.limiters(op._5), Selections.selections(op._2))
        }
        else if (Filters.filters.contains(op._1) && layer.idx == op._4) {
          if (op._1 == "fill") Filters.color = RGBAColor(op._3.toInt)
          else if (op._1 == "median") Filters.dist = op._3.toInt
          else if (op._1 == "wam") {
            Filters.dist = op._3.toInt
            Filters.weights = utils.getMatrixFromOperationsString(op._5, op._3.toInt)
          }
          layer.image.applyFilter(Filters.filter(op._1), Selections.selections(op._2))
        }
      }
      layer.image.applyOperation(Operations.operation("identity", 0), Operations.limit, Selections.getDefaultSelection())
    }
  }
  
  def applyComposition(compositionName: String) {  
    val compOp = Filters.compositionOperations(compositionName)
    for (layer <- layers if layer.isActive) {
      val currentSelection = Selections.getActiveSelection()
      layer.image.applyFilter(compOp, currentSelection)
      
      for (op <- Filters.compositionOutputTemplate(compositionName)) {
        layer.operationsApplied += ((op._1, currentSelection.name, op._3, layer.idx, op._5))
        allOperations += ((op._1, currentSelection.name, op._3, layer.idx, op._5))
      }
    }
  }
}