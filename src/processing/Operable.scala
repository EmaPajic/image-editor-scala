package processing

import image._
import gui._
import scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import processing.Operations._
import utils._

trait Operable {
  def getSelection(): Selection = {
    Selections.getActiveSelection()
  }
  
  val compositionOperations: HashMap[String, Array[Array[Array[Double]]] => Array[Array[Array[Double]]]] = HashMap()
  val compositionOutputTemplate: HashMap[String, ListBuffer[(String, String, Double, Int, String)]] = HashMap()
  
  def composeTwo(f: Array[Array[Array[Double]]] => Array[Array[Array[Double]]], g: Array[Array[Array[Double]]] => Array[Array[Array[Double]]]): Array[Array[Array[Double]]] => Array[Array[Array[Double]]] = {
    f andThen g
  }
   
  def compose(compositionName: String, operationNames: ListBuffer[String], consts: ListBuffer[String]): Unit = {
    val toCompose: ListBuffer[Array[Array[Array[Double]]] => Array[Array[Array[Double]]]] = ListBuffer()
    for (i <- 0 until operationNames.size) {
      if (operations.contains(operationNames(i))) {
        toCompose += operationC(operationNames(i), consts(i).toDouble)
        
        val op: (String, String, Double, Int, String) = (operationNames(i), "whole_image", consts(i).toDouble, 0, "noLimit")
        if (!compositionOutputTemplate.contains(compositionName)) {
          compositionOutputTemplate += ((compositionName, ListBuffer()))
        }
        compositionOutputTemplate(compositionName) += op
      }
      else if (Filters.filters.contains(operationNames(i))) {
        val op: (String, String, Double, Int, String) = (operationNames(i), "whole_image", 0.0, 0, "")
        
        if (operationNames(i) == "fill") {
          val colorComponents = consts(i).split(",")
          Filters.color = RGBAColor(colorComponents(0).toInt, colorComponents(1).toInt, colorComponents(2).toInt)
          
          val op: (String, String, Double, Int, String) = (operationNames(i), "whole_image", Filters.color.toInt, 0, "#")
          if (!compositionOutputTemplate.contains(compositionName)) {
            compositionOutputTemplate += ((compositionName, ListBuffer()))
          }
          compositionOutputTemplate(compositionName) += op
        }
        else if (operationNames(i) == "median") {
          Filters.dist = consts(i).toInt
          
          val op: (String, String, Double, Int, String) = (operationNames(i), "whole_image", Filters.dist, 0, "#")
          if (!compositionOutputTemplate.contains(compositionName)) {
            compositionOutputTemplate += ((compositionName, ListBuffer()))
          }
          compositionOutputTemplate(compositionName) += op
        }
        else if (operationNames(i) == "wam") {
          val info = consts(i).split(",")
          Filters.dist = info(0).toInt
          Filters.weights = utils.getMatrixFromOperationsString(info(1), info(0).toInt)
          
          val op: (String, String, Double, Int, String) = (operationNames(i), "whole_image", Filters.dist, 0, utils.getOperationsStringFromMatrix(Filters.weights))
          if (!compositionOutputTemplate.contains(compositionName)) {
            compositionOutputTemplate += ((compositionName, ListBuffer()))
          }
          compositionOutputTemplate(compositionName) += op        }
        else if (operationNames(i) == "grayscale") {
          val op: (String, String, Double, Int, String) = (operationNames(i), "whole_image", 0.0, 0, "#")
          
          if (!compositionOutputTemplate.contains(compositionName)) {
            compositionOutputTemplate += ((compositionName, ListBuffer()))
          }
          compositionOutputTemplate(compositionName) += op
        }
        toCompose += Filters.filter(operationNames(i))
      }
    }
    
    compositionOperations += ((compositionName, (toCompose :\ operationC("identity", 0.0))(composeTwo)))
    compositionOperations(compositionName)
  }
  
  def listCompositionNames(): String = {
    val names: StringBuilder = new StringBuilder("")
    for (key <- compositionOperations.keys) {
      names.append(key)
      names.append(", ")
    }
    names.toString()
  }
}