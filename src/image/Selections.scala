package image

import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap, ListBuffer}

object Selections {
  val selections: HashMap[String, Selection] = HashMap()
  
  def addDefaultSelection() {
    val sel = Selections("whole_image", (0, 0), (639, 479))
    sel.activate()
  }
  
  def getDefaultSelection():Selection = {
    selections("whole_image")
  }
  
  def activateSelection(name: String) {
    if (!selections.contains(name)) throw new IllegalArgumentException
    selections(name).activate()
  }
  
  def deactivateSelection(name: String) {
    if (!selections.contains(name)) throw new IllegalArgumentException
    selections(name).deactivate()
  }
  
  def deactivateAllSelections() {
    for (key <- selections.keys) {
      selections(key).deactivate()
    }
  }
  
  def addSelection(name: String, rectangles: ListBuffer[SingleSelection]) {
    selections += ((name, Selection(name, rectangles)))
  }
  
  def deleteSelection(name: String) {
    if (!selections.contains(name) || name == "whole_image") throw new IllegalArgumentException
    selections -= name
  }
  
  def apply(name: String, topLeft: (Int, Int) = (0, 0), bottomRight: (Int, Int) = (0, 0)): Selection = {
    val lb: ListBuffer[SingleSelection] = ListBuffer()
    lb += (SingleSelection(topLeft, bottomRight))
    selections(name) = Selection(name, lb)
    selections(name)
  }
  
  def apply(name: String, rectangles: ListBuffer[SingleSelection]): Selection = {
    if (selections.contains(name)) throw new IllegalArgumentException
    selections += ((name, Selection(name, rectangles)))
    selections(name)
  }
  
  def listSelectionNames(): String = {
    val names: StringBuilder = new StringBuilder("")
    for (key <- selections.keys) {
      if (key != "whole_image") {
        names.append(key)
        names.append(", ")
      }
    }
    names.toString()
  }
  
  def getActiveSelection(): Selection = {
    val activeSelection: Option[Selection] = selections.find(_._2.isActive).map(_._2)
    if (activeSelection.isDefined) {
      activeSelection.get
    } else {
    getDefaultSelection()
    }
  }
}