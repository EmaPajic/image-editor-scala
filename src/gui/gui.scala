package gui

import swing._
import swing.event._
import utils._
import image.{Layer, Image, SingleSelection, Selection, Selections, BlendedImage, RGBAColor}
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Color, Point, GraphicsDevice, GraphicsEnvironment}
import math.{min, max}
import processing.Filters
import java.io.File

case class SingleSelectionEvent(sel: SingleSelection) extends Event
case class SelectionEvent(selection: Selection) extends Event
case class SelectionPopUpEvent(rectangles: ListBuffer[SingleSelection], title: String = "Choose Selection name") extends Event

class ImageCanvas() extends Component {
  preferredSize = new Dimension(640, 480)
  focusable = true
  
  override def paintComponent(g: Graphics2D) {
    val bufferedImage: BufferedImage = BlendedImage.blend().toBufferedImage
    g.drawImage(bufferedImage, 0, 0, 640, 480, 0, 0, bufferedImage.getWidth(), bufferedImage.getHeight(), null)
    this.requestFocus()
  }
  
  var mousePressed: Boolean = false
	var CTRLPressed: Boolean = false
	var selectedRectangles: ListBuffer[SingleSelection] = ListBuffer()
	var clickedPoint: (Int, Int) = (0,0)
	
	listenTo(mouse.clicks)
	listenTo(keys)
	
	reactions += {
		case MousePressed(_, pos, _, _, _) => {
			mousePressed = true
			clickedPoint = (pos.getX().toInt, pos.getY().toInt)
		}
		case MouseReleased(_, pos, _, _, _) => {
			if (mousePressed) {
				mousePressed = false
				val clickedPoint2 = (pos.getX().toInt, pos.getY().toInt)
				val leftTop = (min(clickedPoint._1, clickedPoint2._1), min(clickedPoint._2, clickedPoint2._2))
				val bottomRight = (max(clickedPoint._1, clickedPoint2._1), max(clickedPoint._2, clickedPoint2._2))
				publish(SingleSelectionEvent(SingleSelection(leftTop, bottomRight)))
			}
		}
		case KeyPressed(_, Key.Control, _, _) => {
			CTRLPressed = true
		}
		case KeyReleased(_, Key.Control, _, _) => {
			CTRLPressed = false
			if (selectedRectangles.length > 0)
				publish(SelectionPopUpEvent(selectedRectangles))
		}
		case SingleSelectionEvent(rec) => {
			try {
				val validRec = SingleSelection(BlendedImage.toValidPoint(rec.leftTop), BlendedImage.toValidPoint(rec.rightBottom))
				
				if (validRec.leftTop._1 == validRec.rightBottom._1 || validRec.leftTop._2 == validRec.rightBottom._2) throw new IllegalArgumentException
				
				if(!CTRLPressed)
					publish(SelectionPopUpEvent(ListBuffer(validRec)))
				else
					selectedRectangles.append(validRec)
			} catch {
				case e: Throwable => {
					Dialog.showMessage(this, "Invalid Selection")
				}
			}
		}
		case SelectionPopUpEvent(rectangles, title) => {
			val response = Dialog.showInput(this, title, initial= "")
			response match {
				case Some(s) if s != "" => Selections(s, rectangles)
				case None => {}
			}
			selectedRectangles = ListBuffer()
		}
	}
}

class UI extends MainFrame {
  title = "GUI"
  size = new Dimension(320 * 3, 240 * 3)
  var inputLoaded = false
  val canvas = new ImageCanvas()
  
  contents = new BorderPanel {
		focusable = true
		border = Swing.MatteBorder(8, 8, 8, 8, Color.white)
		add(canvas, BorderPanel.Position.Center)
  }
		
  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Load project from file") {
        if (inputLoaded) { Dialog.showMessage(this, "Project already loaded.")}  
        else {
          val chooser = new FileChooser(new File("/home/user/workspace/Projekat"))
          chooser.title = "Choose file to open"
          chooser.showOpenDialog(menuBar)
          println("Loading project from: " + chooser.selectedFile)
          try {
            utils.readInput(chooser.selectedFile)
          } catch {
            case e: Exception => { Dialog.showMessage(this, "Invalid input.") }
          }
          println("Loading done")
          try {
            BlendedImage.reapplyOps()
          } catch {
            case e: Exception => { Dialog.showMessage(this, "Invalid input operations.") }
          }
          inputLoaded = true
          canvas.repaint()
        }
      })
      
      contents += new MenuItem(Action("Choose images and transparencies") {
        if (inputLoaded) { Dialog.showMessage(this, "Project already loaded.")} 
        else {
        val chooser = new FileChooser(new File("/home/user/workspace/Projekat"))
        chooser.title = "Choose images"
        chooser.multiSelectionEnabled_=(true)
        chooser.showOpenDialog(menuBar)
        
        try {
          val inputLayers: ArrayBuffer[Layer] = ArrayBuffer()
          for(i <- 0 until chooser.selectedFiles.size) {
            val image = Image(chooser.selectedFiles(i).getAbsolutePath)
            val response = Dialog.showInput(this, "Input transparency for " + chooser.selectedFiles(i).getPath(), initial="0.0")
            response match {
              case Some(t) => {
                inputLayers += Layer(i, image, t.toDouble)
              } 
              case None => {
                Dialog.showMessage(this, "Invalid transparency.")
                throw new IllegalArgumentException
              }
            }
          }
          BlendedImage(inputLayers)
        } catch {
          case e: Exception => { Dialog.showMessage(this, "Invalid input.") }
        }
        
        println("Loading done")
        inputLoaded = true
        canvas.repaint()
        }
      })
      
      contents += new MenuItem(Action("Save as") {
        val chooser = new FileChooser(new File("/home/user/workspace/Projekat"))
        chooser.showSaveDialog(menuBar)
        println("Saving project to: " + chooser.selectedFile)
        try {
          utils.saveAs(chooser.selectedFile)
        } catch {
          case e: Exception => { Dialog.showMessage(this, "Saving unsuccessful") }
        }
        println("Saving done")
      })
      
      contents += new MenuItem(Action("Export blend") {
        val chooser = new FileChooser(new File("/home/user/workspace/Projekat"))
        chooser.showSaveDialog(menuBar)
        println("Saving blended image to: " + chooser.selectedFile) 
        try {
          BlendedImage.exportBlend(chooser.selectedFile)
        } catch {
          case e: Exception => { Dialog.showMessage(this, "Export unsuccessful") }
        }
        println("Saving done")
      })
    }
    
    contents += new Menu("Layers") {
      val chooser = new MenuItem("Set active layers")
      contents += chooser
      listenTo(chooser)
      
      reactions += {
        case ButtonClicked(b) if b == chooser => {
          val response: Option[String] = Dialog.showInput(this, "Choose active layers from 1 to " + BlendedImage.layers.size.toString, initial= "")
  				response match {
  					case Some(s) => {
  						try {
  						  BlendedImage.deactivateAllLayers()
  						  for (l <- s.split(","))
  							  BlendedImage.layers(l.toInt - 1).activate
  							Dialog.showMessage(this, "Active layers set.")
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Layer index out of range.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
    }
    
    contents += new Menu("Operations") {
      val addOp = new MenuItem("add") 
      contents += addOp
      listenTo(addOp)
      
      reactions += {
        case ButtonClicked(b) if b == addOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant to add.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("add", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val subOp = new MenuItem("sub") 
      contents += subOp
      listenTo(subOp)
      
      reactions += {
        case ButtonClicked(b) if b == subOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant to subtract.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("sub", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val invsubOp = new MenuItem("invsub") 
      contents += invsubOp
      listenTo(invsubOp)
      
      reactions += {
        case ButtonClicked(b) if b == invsubOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant to inverse subtract.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("invsub", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val mulOp = new MenuItem("mul") 
      contents += mulOp
      listenTo(mulOp)
      
      reactions += {
        case ButtonClicked(b) if b == mulOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant to multiply.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("mul", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val divOp = new MenuItem("div") 
      contents += divOp
      listenTo(divOp)
      
      reactions += {
        case ButtonClicked(b) if b == divOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant to divide.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("div", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val invdivOp = new MenuItem("invdiv") 
      contents += invdivOp
      listenTo(invdivOp)
      
      reactions += {
        case ButtonClicked(b) if b == invdivOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant to inverse divide.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("invdiv", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val powOp = new MenuItem("pow") 
      contents += powOp
      listenTo(powOp)
      
      reactions += {
        case ButtonClicked(b) if b == powOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant to for pow.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("pow", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val logOp = new MenuItem("log") 
      contents += logOp
      listenTo(logOp)
      
      reactions += {
        case ButtonClicked(b) if b == logOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant for log.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("log", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val absOp = new MenuItem("abs") 
      contents += absOp
      listenTo(absOp)
      
      reactions += {
        case ButtonClicked(b) if b == absOp => {
          BlendedImage.applyOperation("abs", 0.0)
  				canvas.repaint()
        }
      }
      
      val minOp = new MenuItem("min") 
      contents += minOp
      listenTo(minOp)
      
      reactions += {
        case ButtonClicked(b) if b == minOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant for min.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("min", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val maxOp = new MenuItem("max") 
      contents += maxOp
      listenTo(maxOp)
      
      reactions += {
        case ButtonClicked(b) if b == maxOp => {
          val response: Option[String] = Dialog.showInput(this, "Input constant for max.", initial= "")
  				response match {
  					case Some(const) => {
  						try {
  						  BlendedImage.applyOperation("max", const.toDouble)
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val invertOp = new MenuItem("invert") 
      contents += invertOp
      listenTo(invertOp)
      
      reactions += {
        case ButtonClicked(b) if b == invertOp => {
          BlendedImage.applyOperation("invert", 0.0)
  				canvas.repaint()
        }
      }
    }
    
    
    contents += new Menu("Filters") {      
      val grayscaleOp = new MenuItem("Grayscale") 
      contents += grayscaleOp
      listenTo(grayscaleOp)
      
      reactions += {
        case ButtonClicked(b) if b == grayscaleOp => {
          BlendedImage.applyFilter("grayscale")
  				canvas.repaint()
        }
      }
      
      val fillOp = new MenuItem("Fill") 
      contents += fillOp
      listenTo(fillOp)
      
      reactions += {
        case ButtonClicked(b) if b == fillOp => {
          val response: Option[String] = Dialog.showInput(this, "Input color components as r,g,b.", initial= "")
  				response match {
  					case Some(s) => {
  						try {
  						  val colorComponents = s.split(",")
  						  Filters.color = RGBAColor(colorComponents(0).toInt, colorComponents(1).toInt, colorComponents(2).toInt)
  						  BlendedImage.applyFilter("fill")
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val medianOp = new MenuItem("Median filter") 
      contents += medianOp
      listenTo(medianOp)
      
      reactions += {
        case ButtonClicked(b) if b == medianOp => {
          val response: Option[String] = Dialog.showInput(this, "Input distance for median filter.", initial= "")
  				response match {
  					case Some(dist) => {
  						try {
  						  Filters.dist = dist.toInt
  						  BlendedImage.applyFilter("median")
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
      
      val wamOp = new MenuItem("WAM filter") 
      contents += wamOp
      listenTo(wamOp)
      
      reactions += {
        case ButtonClicked(b) if b == wamOp => {
          val response: Option[String] = Dialog.showInput(this, "Input distance and weights matrix for wam filter.", initial= "")
  				response match {
  					case Some(s) => {
  						try {
  						  val info = s.split(",")
  						  Filters.dist = info(0).toInt
  						  Filters.weights = utils.getMatrixFromOperationsString(info(1), info(0).toInt)
  						  BlendedImage.applyFilter("wam")
  							canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid input.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
    }
    
    contents += new Menu("Selections") {      
      val chooseSelOp = new MenuItem("Choose active selection") 
      contents += chooseSelOp
      listenTo(chooseSelOp)
      
      reactions += {
        case ButtonClicked(b) if b == chooseSelOp => {
          val response: Option[String] = Dialog.showInput(this, "Choose selection from: " + Selections.listSelectionNames(), initial= "")
  				response match {
  					case Some(sel) => {
  						try {
  						  Selections.deactivateAllSelections()
  						  Selections.activateSelection(sel)
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid selection name.")
  						}
  					}
  					case _ => {
  					  Selections.deactivateAllSelections()
  					}
  				}
        }
      }
      
      val deleteSelOp = new MenuItem("Delete selection") 
      contents += deleteSelOp
      listenTo(deleteSelOp)
      
      reactions += {
        case ButtonClicked(b) if b == deleteSelOp => {
          val response: Option[String] = Dialog.showInput(this, "Choose selection to delete from: " + Selections.listSelectionNames(), initial= "")
  				response match {
  					case Some(sel) => {
  						try {
  						  BlendedImage.deleteSelectionOps(sel)
  						  Selections.deleteSelection(sel)
  						  BlendedImage.resetImages()
  						  BlendedImage.reapplyOps()
  						  canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid selection name.")
  						}
  					}
  					case _ => { Dialog.showMessage(this, "Invalid input.") }
  				}
        }
      }
    }
    
    contents += new Menu("Compositions") {
      val compositionOp = new MenuItem("Apply new composition") 
      contents += compositionOp
      listenTo(compositionOp)
      
      reactions += {
        case ButtonClicked(b) if b == compositionOp => {
          try {
            var numOps = 0
            var compName = ""
            val numops: Option[String] = Dialog.showInput(this, "Input name of composition and number of operations in compositions: ", initial= "")
    				numops match {
    					case Some(s) => {
    						try {
    						  val info = s.split(",")
    						  numOps = info(1).toInt
    						  compName = info(0)
    						} catch {
    							case e: IllegalArgumentException => Dialog.showMessage(this, "Invalid input.")
    						}
    					}
    					case _ => { Dialog.showMessage(this, "Invalid input.") }
    				}
            
            try {
              val ops: ListBuffer[String] = ListBuffer()
              val consts: ListBuffer[String] = ListBuffer()
              for (i <- 0 until numOps) {
                val response = Dialog.showInput(this, "Input operation, then / and then its arguments separated with comma.", initial="")
                response match {
                  case Some(t) => {
                    val info = t.split("/")
                    info(0) match {
                      case "add" | "sub" | "invsub" | "mul" | "div" | "invdiv" | "pow" | "log" | "min" | "max" | "fill" | "median" | "wam" => {
                        ops += info(0)
                        consts += info(1)
                      }
                      case "abs" | "invert" | "grayscale" => {
                        ops += info(0)
                        consts += "0.0"
                      }
                      case _ => {
                        Dialog.showMessage(this, "Invalid input.")
                      }
                    }
                  } 
                  case None => {
                    Dialog.showMessage(this, "Invalid input.")
                    throw new IllegalArgumentException
                  }
                }
              }
              
              Filters.compose(compName, ops, consts)
              BlendedImage.applyComposition(compName)
              canvas.repaint()
              println("Composite operation done")
            } catch {
              case e: IllegalArgumentException => Dialog.showMessage(this, "Invalid input.")
            }
          }
        catch {
          case e: Exception => { Dialog.showMessage(this, "Invalid input.") }
          }
        }
      }
      
      val chooseCompOp = new MenuItem("Apply existing composition") 
      contents += chooseCompOp
      listenTo(chooseCompOp)
      
      reactions += {
        case ButtonClicked(b) if b == chooseCompOp => {
          val response: Option[String] = Dialog.showInput(this, "Choose composition from: " + Filters.listCompositionNames(), initial= "")
  				response match {
  					case Some(compName) => {
  						try {
  						  BlendedImage.applyComposition(compName)
                canvas.repaint()
  						} catch {
  							case e: Exception => Dialog.showMessage(this, "Invalid selection name.")
  						}
  					}
  					case _ => {
  					  Dialog.showMessage(this, "Invalid input.")
  					}
  				}
        }
      }
    }
  }
  peer.setLocationRelativeTo(null)
}

object gui {
  def main(args: Array[String]) {
    val ui = new UI
    ui.visible = true
    Selections.addDefaultSelection()
    println("End of main function")
  }
}
