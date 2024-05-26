package game

import game.Blox.{BASIC, EMPTY, EMPTY2, END, SPECIAL, START}
import game.GameMediaPlayer.Sounds
import game.GameStatus.{Fail, Game, GameStatus, Win}
import game.Moves.{Down, Left, Moves, Right, Up}
import game.PauseType.{FailScene, Pause, PauseType, WinScene}
import javafx.scene.input.KeyCode
import javafx.scene.paint
import javafx.scene.paint.ImagePattern
import scalafx.scene.shape.Rectangle
import javafx.util.Duration
import scalafx.application.JFXApp3
import scalafx.scene.{Camera, Group, Node, Scene, SceneAntialiasing, SubScene}
import scalafx.scene.control.{Button, ContextMenu, MenuItem, TextInputDialog}
import scalafx.scene.paint._
import scalafx.Includes._
import scalafx.animation.{ParallelTransition, ScaleTransition, SequentialTransition, TranslateTransition}
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.MouseEvent._
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.Text
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.FileChooser
import scalafx.scene.shape.Box

import java.awt.Robot
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

final case class DimensionsException(private val message: String = "Matrix should be 14x14") extends Exception(message)

final case class UnsupportedCharacter(private val message: String = "Unsupported character") extends Exception(message)

final case class StartBloxException(private val message: String = "No start blox") extends Exception(message)

object Blox {

  def createMaterial(color: Color, image: String = null): javafx.scene.paint.PhongMaterial = new PhongMaterial() {
    if (image != null) diffuseMap = new Image(image)
    else diffuseColor = color
  }

  val SPECIAL: paint.PhongMaterial = createMaterial(Color.AliceBlue)
  val BASIC: paint.PhongMaterial = createMaterial(Color.Gray, "basic2.jpg")
  val EMPTY: paint.PhongMaterial = createMaterial(Color.Transparent)
  val EMPTY2: paint.PhongMaterial = createMaterial(Color.Black)
  val START: paint.PhongMaterial = createMaterial(Color.FireBrick)
  val END: paint.PhongMaterial = createMaterial(Color.OrangeRed)
}

object Moves extends Enumeration {
  type Moves = Value
  val Left, Right, Down, Up = Value
}

object PauseType extends Enumeration {
  type PauseType = Value
  val WinScene, FailScene, Pause = Value
}

object GameStatus extends Enumeration {
  type GameStatus = Value
  val Game, Win, Fail = Value
}

object Bloxorz extends JFXApp3 {

  final val bloxSize: Int = 55
  private final val tableSize = 14
  final val sceneHeight = 768
  final val sceneWidth = 1024
  private final val playGroundSize = bloxSize * tableSize

  def positionCamera(camera: PerCamera, state: State): PerCamera = {
    val blox: List[(Double, Double)] = state.blox
    val end: (Double, Double) = state.endPos
    def minX: Double = min(blox.head._1, blox.tail.head._1).doubleValue()
    def minY: Double = min(blox.head._2, blox.tail.head._2).doubleValue()

    if (math.abs(minX - end._1) / bloxSize > 5 || camera.getCam.translateX.getValue < sceneWidth / 4) {
      camera.getCam.translateX = minX + sceneWidth / 4
    }
    if (math.abs(minY - end._2) / bloxSize > 5 || camera.getCam.translateY.getValue < sceneHeight / 5) {
      camera.getCam.translateY = minY + sceneHeight / 6
    }
    camera
  }

  def getLinesFromFile(src: String): List[String] = {
    val bufferedSource = Source.fromFile(src)
    val str: List[String] = bufferedSource.getLines().toList
    bufferedSource.close
    str
  }

  def getRectangles(src: List[String], _create: Boolean = false): List[Box] = {
    if (src.length != tableSize) throw DimensionsException()
    for (line: String <- src if line.length != tableSize) throw DimensionsException()

    @tailrec
    def getRectangles4(i: Int, j: Int, acc: List[Box]): List[Box] = {
      (i, j) match {
        case (this.tableSize, 0) => acc
        case (this.tableSize, e) => getRectangles4(0, e - 1, acc)
        case (r, e) => getRectangles4(r + 1, e, matchRectangle(src(j).charAt(i), i, e) :: acc)
      }
    }

    def matchRectangle(e: Char, j: Int, i: Int): Box = {
      if (_create && (e.equals('–') || e.equals('-'))) return new CustomizedBox(j * bloxSize, i * bloxSize, EMPTY2)
      if (_create && e.equals('S')) return new CustomizedBox(j * bloxSize, i * bloxSize, START)
      e.toUpper match {
        case '–' => new CustomizedBox(j * bloxSize, i * bloxSize, EMPTY)
        case '-' => new CustomizedBox(j * bloxSize, i * bloxSize, EMPTY)
        case 'O' => new CustomizedBox(j * bloxSize, i * bloxSize, BASIC)
        case 'S' => new CustomizedBox(j * bloxSize, i * bloxSize, BASIC)
        case 'T' => new CustomizedBox(j * bloxSize, i * bloxSize, END)
        case '.' => new CustomizedBox(j * bloxSize, i * bloxSize, SPECIAL)
        case _ => throw UnsupportedCharacter()
      }
    }

    getRectangles4(0, tableSize - 1, List[Box]())
  }

  def mapKeyCodes(key: KeyCode): Moves = key match {
    case KeyCode.W => Up
    case KeyCode.S => Down
    case KeyCode.A => Left
    case KeyCode.D => Right
    case KeyCode.UP => Up
    case KeyCode.DOWN => Down
    case KeyCode.LEFT => Left
    case KeyCode.RIGHT => Right
  }

  def getInitialBlox(src: List[String]): List[(Double, Double)] = {
    for ((line: String, i) <- src.zipWithIndex; (e, j) <- line.toUpperCase.zipWithIndex) {
      if (e.toUpper == 'S') return (j * bloxSize * 1.0, i * bloxSize * 1.0) :: ((j * bloxSize * 1.0, i * bloxSize * 1.0) :: List[(Double, Double)]())
    }
    throw StartBloxException()
  }

  def getBlocks(src: String): (List[(Double, Double)], (Double, Double), List[(Double, Double)], List[(Double, Double)]) = { // init, end, empty space, special blocks, end_point
    val arena = getLinesFromFile(src)
    val initialBlox2 = getInitialBlox(arena)
    val (empty1: List[Box], s) = getRectangles(arena).partition(p => p.getMaterial == EMPTY)
    val (special1, s2) = s.partition(p => p.getMaterial == SPECIAL)
    val empty2: List[(Double, Double)] = empty1.map(r => (r.translateX.value, r.translateY.value))
    val spec2: List[(Double, Double)] = special1.map(r => (r.translateX.value, r.translateY.value))
    val endPos2: (Double, Double) = s2.filter(p => p.getMaterial == END).map(r => (r.translateX.value, r.translateY.value)).head
    (initialBlox2, endPos2, empty2, spec2)
  }

  case class State(gameStatus: GameStatus, src: String, blox: List[(Double, Double)], endPos: (Double, Double), emptyBloxList: List[(Double, Double)], specialBloxList: List[(Double, Double)]) {
    def newState(dir: Moves): State = {
      val (newx1, newy1, newx2, newy2) = calculatePosition(blox.head._1, blox.head._2, blox.tail.head._1, blox.tail.head._2, dir)
      if (newx1 < 0 || newx1 >= playGroundSize || newy1 < 0 || newy1 >= playGroundSize ||
        newx2 < 0 || newx2 >= playGroundSize || newy2 < 0 || newy2 >= playGroundSize ||
        (newx1 == newx2 && newy1 == newy2 && specialBloxList.contains((newx1, newy1))) ||
        emptyBloxList.contains((newx1, newy1)) || emptyBloxList.contains((newx2, newy2))) {
        State(Fail, src, List((newx1, newy1), (newx2, newy2)), endPos, emptyBloxList, specialBloxList)
      }
      else if (newx1 == newx2 && (newx2 == endPos._1) && newy1 == newy2 && newy2 == endPos._2) {
        State(Win, src, List((newx1, newy1), (newx2, newy2)), endPos, emptyBloxList, specialBloxList)
      }
      else {
        State(Game, src, List((newx1, newy1), (newx2, newy2)), endPos, emptyBloxList, specialBloxList)
      }
    }

    def rectangles: (List[Box], Box) = {
      val bloxBox = new CustomizedBox(min(blox.head._1, blox.tail.head._1).intValue(), min(blox.head._2, blox.tail.head._2).intValue(), START,
        (blox.head._1 == blox.tail.head._1, blox.head._2 == blox.tail.head._2) match {
          case (true, true) => BloxPosition.Z_axis
          case (true, false) => BloxPosition.Y_axis
          case (false, true) => BloxPosition.X_axis
        })
      (List(bloxBox) ::: getRectangles(getLinesFromFile(src)), bloxBox)
    }
  }

  def chooseLevelMenu(play: Boolean): Unit = { // true -play false - edit
    val stop = 4
    val vBox = new VBox(0)
    var hBox = new HBox(0)
    hBox.alignment = Pos.BaselineCenter

    def createMapButton(_width: Double, _height: Double, level: Int) = new Button() {
      shape = new Rectangle() {
        width = _width
        height = _height
        arcHeight = 40
        arcWidth = 40
      }
      graphic = new ImageView() {
        image = new Image("levelTraining" + level + ".png")
      }
      prefWidth = _width
      prefHeight = _height
      alignmentInParent = level % 4 match {
        case 1 => Pos.TopLeft
        case 2 => Pos.TopRight
        case 3 => Pos.BottomLeft
        case 0 => Pos.BottomRight
      }
      onAction = () => {
        Sounds.click.play()
        if (play) loadLevel("levelTraining" + level + ".txt", fromFile = false)
        else createLevel(level)
      }
    }

    @tailrec
    def createButtons(level: Int = 1): Unit = {
      if (level <= stop && Files.exists(Paths.get("levelTraining" + level + ".txt"))) {
        hBox.getChildren.add(createMapButton(sceneWidth / 2, sceneHeight / 2, level))
        if (hBox.getChildren.size() == 2) {
          vBox.getChildren.add(hBox)
          hBox = new HBox()
        }
        createButtons(level + 1)
      }
    }

    createButtons()
    if (hBox.getChildren.size > 0) vBox.getChildren.add(hBox)
    stage.scene.value.content = vBox
  }

  def saveFileAs(): File = {
    val fileChooser: FileChooser = new FileChooser
    fileChooser.setTitle("Save As")
    fileChooser.getExtensionFilters.add(new ExtensionFilter("Text", "*.txt"))
    fileChooser.showSaveDialog(stage)
  }

  def openFile(): File = {
    val fileChooser: FileChooser = new FileChooser
    fileChooser.getExtensionFilters.add(new ExtensionFilter("Text", "*.txt"))
    fileChooser.showOpenDialog(stage)
  }

  def findSolution(src: String): Unit = {
    val saveFile = saveFileAs()
    if (saveFile != null) {

      val writer = new PrintWriter(new File(saveFile.getAbsolutePath))
      val (blox, end, empty, spec) = getBlocks(src) ////////////

      def nextPosition(state: (Double, Double, Double, Double)): List[(Double, Double, Double, Double)] = {
        val acc: ListBuffer[(Double, Double, Double, Double)] = ListBuffer[(Double, Double, Double, Double)]()
        for (dir <- Moves.values) {
          val (newx1, newy1, newx2, newy2) = calculatePosition(state._1, state._2, state._3, state._4, dir)
          if (newx1 >= 0 && newx1 < bloxSize * tableSize && newy1 >= 0 && newy1 < bloxSize * tableSize &&
            newx2 >= 0 && newy2 >= 0 && newy2 < bloxSize * tableSize && !(empty.contains((newx1, newy1)) || empty.contains((newx2, newy2))) &&
            !(spec.contains((newx1, newy1)) && (newx1, newy1) == (newx2, newy2))) {
            acc += ((newx1, newy1, newx2, newy2))
          }
        }
        acc.toList
      }

      @tailrec
      def writeToFile(current: Int, state: List[(Double, Double, Double, Double)], accFrom: List[Int]): Unit = {
        if (current == 0) writer.close()
        else {
          if (state(accFrom(current))._1 < state(current)._1) writer.write("down\n")
          else if (state(accFrom(current))._1 > state(current)._1) writer.write("up\n") //
          else if (state(accFrom(current))._2 < state(current)._2) writer.write("left\n")
          else if (state(accFrom(current))._2 > state(current)._2) writer.write("right\n") //
          writeToFile(accFrom(current), state, accFrom)
        }
      }

      @tailrec
      def solveLevel(current: Int, state: List[(Double, Double, Double, Double)], accFrom: List[Int], end: (Double, Double)): Unit = {
        if (state.length < current + 1) {
          writer.write("This one has no solution")
          writer.close()
        }
        else if (state(current)._1 == state(current)._3 && state(current)._2 == state(current)._4 && state(current)._1 == end._1 && state(current)._2 == end._2) {
          writeToFile(current, state, accFrom)
        }
        else {
          val nxt = nextPosition(state(current)).filterNot(p => state.contains(p))
          solveLevel(current + 1, state ::: nxt, accFrom ::: List.fill(nxt.length)(current), end)
        }
      }

      solveLevel(0, List((end._1, end._2, end._1, end._2)), List(0), blox.head)
    }
  }

  def calculatePosition(x1: Double, y1: Double, x2: Double, y2: Double, direction: Moves): (Double, Double, Double, Double) = {
    direction match {
      case Left if x1 == x2 && y1 == y2 => (x1, y1 - bloxSize, x2, y2 - 2 * bloxSize)
      case Left if x1 != x2 && y1 == y2 => (x1, y1 - bloxSize, x2, y2 - bloxSize)
      case Left if x1 == x2 && y1 < y2 => (x1, y1 - bloxSize, x2, y2 - 2 * bloxSize)
      case Left if x1 == x2 && y2 < y1 => (x1, y1 - 2 * bloxSize, x2, y2 - bloxSize)

      case Right if x1 == x2 && y1 == y2 => (x1, y1 + bloxSize, x2, y2 + 2 * bloxSize)
      case Right if x1 != x2 && y1 == y2 => (x1, y1 + bloxSize, x2, y2 + bloxSize)
      case Right if x1 == x2 && y1 < y2 => (x1, y1 + 2 * bloxSize, x2, y2 + bloxSize)
      case Right if x1 == x2 && y2 < y1 => (x1, y1 + bloxSize, x2, y2 + 2 * bloxSize)

      case Down if x1 == x2 && y1 == y2 => (x1 - bloxSize, y1, x2 - 2 * bloxSize, y2)
      case Down if x1 < x2 && y1 == y2 => (x1 - bloxSize, y1, x2 - 2 * bloxSize, y2)
      case Down if x1 > x2 && y1 == y2 => (x1 - 2 * bloxSize, y1, x2 - bloxSize, y2)
      case Down if x1 == x2 && y2 != y1 => (x1 - bloxSize, y1, x2 - bloxSize, y2)

      case Up if x1 == x2 && y1 == y2 => (x1 + bloxSize, y1, x2 + 2 * bloxSize, y2)
      case Up if x1 < x2 && y1 == y2 => (x1 + 2 * bloxSize, y1, x2 + bloxSize, y2)
      case Up if x1 > x2 && y1 == y2 => (x1 + bloxSize, y1, x2 + 2 * bloxSize, y2)
      case Up if x1 == x2 && y2 != y1 => (x1 + bloxSize, y1, x2 + bloxSize, y2)

      case _ => (x1, y1, x2, y2)
    }
  }

  def createLevel(level: Int): Unit = {

    val rectangles: List[Box] = getRectangles(getLinesFromFile("levelTraining" + level + ".txt"), _create = true)
    createGroupHierarchy_returnRoot(rectangles, List(CustomizedButton.mainMenuButtonSmall, new CustomizedButton("SAVE", () => {
      val saveFile = saveFileAs()
      if (saveFile != null) {
        val writer = new PrintWriter(new File(saveFile.getAbsolutePath))
        @tailrec
        def writeMap(i: Int = tableSize - 1, j: Int = 0, max: Int = tableSize): Unit = {
          (i, j) match {
            case (-1, 13) => writer.close()
            case (-1, _) => writer.write("\n")
              writeMap(max - 1, j + 1)
            case (_, _) =>
              val m = rectangles(i + j * max).getMaterial
              if (m.equals(SPECIAL)) writer.write(".")
              else if (m.equals(BASIC)) writer.write("o")
              else if (m.equals(END)) writer.write("T")
              else if (m.equals(START)) writer.write("S")
              else if (m.equals(EMPTY2)) writer.write("-")
              writeMap(i - 1, j)
          }
        }
        writeMap()
        createMainMenu()
      }
    }, _prefWidth = 100, _translateX = Bloxorz.sceneWidth - 113, _translateY = Bloxorz.sceneHeight/2)),
    new ParCamera( -bloxSize / 2 - (sceneWidth - bloxSize * tableSize) / 2, -bloxSize / 2),parallelCamera = true)
    def col(rectangle: Box, color: javafx.scene.paint.PhongMaterial): Unit = {
      @tailrec
      def changeColor(tempRectangles: List[Box]): Unit = {
        if (tempRectangles == null || tempRectangles.isEmpty) return
        if (tempRectangles.head.getMaterial.equals(color)) tempRectangles.head.setMaterial(BASIC)
        changeColor(tempRectangles.tail)
      }

      changeColor(rectangles)
      rectangle.setMaterial(color)
    }

    def invertMap(): Unit = rectangles.foreach(r => {
      if (r.getMaterial == END) r.setMaterial(START)
      else if (r.getMaterial == START) r.setMaterial(END)
    })

    def replaceSpecial(): Unit = rectangles.foreach(r => {
      if (r.getMaterial == SPECIAL) r.setMaterial(BASIC)
    })

    def isOnEdge(num: Int, color: List[Material]): Boolean = {
      if (num < tableSize * (tableSize - 1) && color.contains(rectangles(num + tableSize).getMaterial)) return true
      if (num % tableSize < (tableSize - 1) && color.contains(rectangles(num + 1).getMaterial)) return true
      if (num % tableSize > 0 && color.contains(rectangles(num - 1).getMaterial)) return true
      if (num > (tableSize - 1) && color.contains(rectangles(num - tableSize).getMaterial)) return true
      false
    }

    def filterRadius(radius: Int, num: Int): Unit = {
      val x = num / tableSize
      val y = num % tableSize
      val xmin = if (x - radius < 0) 0 else x - radius
      val ymin = if (y - radius < 0) 0 else y - radius
      val xmax = if (x + radius > (tableSize - 1)) tableSize - 1 else x + radius
      val ymax = if (y + radius > (tableSize - 1)) tableSize - 1 else y + radius
      for (i <- xmin to xmax; j <- ymin to ymax if rectangles(i * tableSize + j).getMaterial == SPECIAL) {
        rectangles(i * tableSize + j).setMaterial(BASIC)
      }
    }

    def createMenuItem(name: String, function: () => Unit) = new MenuItem {
      text = name
      onAction = () => {
        function()
      }
    }

    rectangles.indices.foreach(i => {
      val r = rectangles(i)
      r.handleEvent(MouseEntered) {
        _: MouseEvent => {
          r.width = bloxSize - 5
          r.height = bloxSize - 5
        }
      }
      r.handleEvent(MouseExited) {
        _: MouseEvent => {
          r.width = bloxSize
          r.height = bloxSize
        }
      }
      val deleteBasic: MenuItem = createMenuItem("Delete", () => r.setMaterial(EMPTY2))
      val addBasic: MenuItem = createMenuItem("Add", () => r.setMaterial(BASIC))
      val special: MenuItem = createMenuItem("Set Special", () => r.setMaterial(SPECIAL))
      val basic: MenuItem = createMenuItem("Set Basic", () => r.setMaterial(BASIC))
      val start: MenuItem = createMenuItem("Set Start", () => col(r, START))
      val end: MenuItem = createMenuItem("Set End", () => col(r, END))
      val invert: MenuItem = createMenuItem("Invert", () => invertMap())
      val removeSpec: MenuItem = createMenuItem("Remove Special", () => replaceSpecial())
      val filter: MenuItem = createMenuItem("Filter", () => {
        val dialog = new TextInputDialog(defaultValue = "1") {
          initOwner(stage)
          graphic = null
          title = "Filter"
          contentText = "Set Radius"
        }
        dialog.setHeaderText(null)

        val result = dialog.showAndWait()
        result match {
          case Some(name) =>
            name.toIntOption match {
              case Some(radius: Int) => filterRadius(radius, i)
              case None =>
            }
          case None => println("Dialog was canceled.")
        }
      })

      r.handleEvent(MousePressed) {
        a: MouseEvent => {
          val contextMenu: ContextMenu = new ContextMenu()
          if (a.secondaryButtonDown) contextMenu.getItems.addAll(invert, removeSpec)
          else {
            if (r.getMaterial.equals(EMPTY2)) {
              contextMenu.getItems.add(filter)
              if (isOnEdge(i, List(BASIC, SPECIAL, END, START))) contextMenu.getItems.addAll(addBasic)
            }
            else if (r.getMaterial.equals(START)) {}
            else if (r.getMaterial.equals(BASIC)) {
              if (isOnEdge(i, List(EMPTY2))) contextMenu.getItems.addAll(deleteBasic)
              contextMenu.getItems.addAll(special, start, end)
            }
            else if (r.getMaterial.equals(SPECIAL)) contextMenu.getItems.addAll(basic, filter)
          }
          contextMenu.show(stage, a.screenX, a.screenY)
        }
      }
    })
  }

  def loadLevel(level: Int): Unit = {
    if (Files.exists(Paths.get("level" + level + ".txt"))) {
      loadLevel("level" + level + ".txt", fromFile = false, trainingMode = false, level)
    }
    else createWinMenu1()
  }

  private def createGroupHierarchy_returnRoot(rectangle: List[Box], uiComponents: List[Node], _camera: Camera, parallelCamera:Boolean = false): SubScene = {
    val world = new Group{ children = rectangle }
    val subScene3D = new SubScene(world, 1024, 768, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {camera = _camera}
    stage.scene = new Scene(new Group{ children = uiComponents ::: List(subScene3D) }, 1024, 768, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
      fill = if (parallelCamera) { Color.White} else {
        new ImagePattern(new Image("background2.jpg"), 0, 0, 1, 1, true)}
    }
    subScene3D
  }

  def loadLevel(src: String, fromFile: Boolean, trainingMode: Boolean = true, level: Int = 1, _state: State = null, objectProperty: ObjectProperty[PerCamera] = null): Unit = {
    var selectedFile: File = null
    if (fromFile) selectedFile = openFile()
    if (fromFile && selectedFile == null) return // if window for loading level is closed

    val (initialBlox, end, empty, spec) = getBlocks(src)
    val state = if (_state == null) ObjectProperty(State(Game, src, initialBlox, end, empty, spec)) // startLevel
    else ObjectProperty(_state) // continue After Pause With Previous State

    val _camera = positionCamera(new PerCamera(true), state.value)
    val subScene3D = createGroupHierarchy_returnRoot(state.value.rectangles._1, List(MenuHelper.numOfMoves, MenuHelper.pauseButton), _camera)
    if (!trainingMode) MenuHelper.pauseButton.onMouseClicked = () => pauseMenuCompetitionMode(src, level, state.value, ObjectProperty(_camera)) // save state and camera position on pause
    else MenuHelper.pauseButton.onMouseClicked = () => pauseMenuTraining(Pause, src, state.value, ObjectProperty(_camera))
    if (_state != null) {
      _camera.getCam.translateX = objectProperty.value.getCam.translateX.toDouble
      _camera.getCam.translateY = objectProperty.value.getCam.translateY.toDouble
    } // fetching and setting last camera position if level is continued

    def playFromFile(): Unit =
      new Thread {
        override def run(): Unit = {
          val robot: Robot = new Robot {
            setAutoDelay(300)
          }
          robot.delay(200)
          for (line: String <- getLinesFromFile(selectedFile.getAbsolutePath)) {
            line.toUpperCase() match {
              case "LEFT" => robot.keyPress(java.awt.event.KeyEvent.VK_LEFT)
                robot.keyRelease(java.awt.event.KeyEvent.VK_LEFT)
              case "RIGHT" => robot.keyPress(java.awt.event.KeyEvent.VK_RIGHT)
                robot.keyRelease(java.awt.event.KeyEvent.VK_RIGHT)
              case "DOWN" => robot.keyPress(java.awt.event.KeyEvent.VK_DOWN)
                robot.keyRelease(java.awt.event.KeyEvent.VK_DOWN)
              case "UP" => robot.keyPress(java.awt.event.KeyEvent.VK_UP)
                robot.keyRelease(java.awt.event.KeyEvent.VK_UP)
            }
          }
        }
      }.start()

    def startLevel(): Unit = {
      def disableEventHandler(): Unit = stage.scene.value.onKeyPressed = null

      stage.scene.value.onKeyPressed = key => {
        if (key.getCode.equals(KeyCode.ESCAPE) && !trainingMode) pauseMenuCompetitionMode(src, level, state.value, ObjectProperty(_camera))
        else if (key.getCode.equals(KeyCode.ESCAPE) && trainingMode) pauseMenuTraining(Pause, src, state.value, ObjectProperty(_camera))
        if (List(KeyCode.W, KeyCode.S, KeyCode.A, KeyCode.D, KeyCode.UP, KeyCode.DOWN, KeyCode.LEFT, KeyCode.RIGHT).contains(key.getCode)) {
          Sounds.block.play()
          MenuHelper.incNumOfMoves()
          state.update(state.value.newState(mapKeyCodes(key.getCode)))
          val (content, blox) = state.value.rectangles
          subScene3D.content = content
          positionCamera(_camera, state.value)
          state.value.gameStatus match {
            case Fail => disableEventHandler()
              new SequentialTransition(blox, Seq(
                new ParallelTransition(blox, (new TransformCalculations).calculate(mapKeyCodes(key.getCode), state.value.blox, state.value.emptyBloxList, blox)),
                new TranslateTransition(Duration.seconds(0.5), blox) { toZ = bloxSize })) {
                onFinished = () => {
                  if (trainingMode) pauseMenuTraining(FailScene, src)
                  else loadLevel(level) }
              }.play()
            case Win => disableEventHandler()
              new ParallelTransition(blox, Seq(
                new TranslateTransition(Duration.seconds(1)) { toZ = -bloxSize / 6 },
                new ScaleTransition(Duration.seconds(1)) { toZ = 0 })){
                onFinished = () => {
                  if (trainingMode) pauseMenuTraining(WinScene, src)
                  else loadLevel(level + 1) }
              }.play()
            case _ => ()
          }
        }
      }
    }
    startLevel()
    if (fromFile) playFromFile()
  }

  def createMenu(list: List[Node]): Unit =
    stage.scene = new Scene(sceneWidth, sceneHeight, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
      fill = Color.rgb(38, 38, 38)
      content = new VBox {
        spacing = 5
        prefWidth = sceneWidth
        prefHeight = sceneHeight
        padding = Insets(55, 0, 0, 0)
        alignment = Pos.BaselineCenter
        alignmentInParent = Pos.BaselineCenter
        children = list
      }
    }

  def pauseMenuCompetitionMode(src: String, level: Int, state: State, objectProperty: ObjectProperty[PerCamera]): Unit = {
    val continue = new CustomizedButton("CONTINUE", () => loadLevel(src, fromFile = false, trainingMode = false, level, state, objectProperty))
    val restart = new CustomizedButton("RESTART LEVEL", () => loadLevel(src, fromFile = false, trainingMode = false, level))
    createMenu(List(MenuHelper.pauseText, continue, restart, CustomizedButton.toggleEffects, CustomizedButton.toggleMusic, CustomizedButton.mainMenuButton))
  }

  def createMainMenu(): Unit = createMenu(List(
    MenuHelper.bloxorzStartText,
    new CustomizedButton("TRAINING", () => {
      MenuHelper.resetNumOfMoves()
      chooseLevelMenu(true)
    }),
    new CustomizedButton("START", () => {
      MenuHelper.resetNumOfMoves()
      loadLevel(1)
    }),
    new CustomizedButton("LOAD LEVEL FROM FILE", () => {
      MenuHelper.resetNumOfMoves()
      val selectedFile = openFile()
      if (selectedFile != null)  loadLevel(selectedFile.getAbsolutePath, fromFile = false)
    }),
    new CustomizedButton("CREATE LEVEL", () => chooseLevelMenu(false)),
    CustomizedButton.toggleEffects,
    CustomizedButton.toggleMusic,
    new CustomizedButton("HIGH SCORES", () => createHghScoresMenu(createMainMenu)),
    new CustomizedButton("QUIT", stage.close)
  ))

  def pauseMenuTraining(pauseType: PauseType, src: String, state: State = null, objectProperty: ObjectProperty[PerCamera] = null): Unit = {
    pauseType match {
      case WinScene => Sounds.win.play()
      case FailScene => Sounds.lost.play()
      case _ => ()}
    val continue = new CustomizedButton("CONTINUE", () => loadLevel(src, fromFile = false, trainingMode = true, 1, state, objectProperty))
    val again = new CustomizedButton(
      pauseType match {
        case Pause => "RESTART"
        case _ => "AGAIN?"
      }, () => {
        MenuHelper.resetNumOfMoves()
        loadLevel(src, fromFile = false)
      })
    val solution = new CustomizedButton("SOLUTION", () => findSolution(src))
    val sequence = new CustomizedButton("INSERT SEQUENCE FROM FILE", () => {
      MenuHelper.resetNumOfMoves()
      loadLevel(src, fromFile = true)
    })
    val text: Text = pauseType match {
      case WinScene => MenuHelper.winText
      case FailScene => MenuHelper.failText
      case Pause => MenuHelper.pauseText
    }
    val list: List[Node] = List(again, solution, sequence, CustomizedButton.toggleEffects, CustomizedButton.toggleMusic, CustomizedButton.mainMenuButton)
    if (pauseType.equals(Pause)) createMenu(text :: continue :: list)
    else createMenu(text :: list)
  }

  def createWinMenu1(): Unit = {
    MenuHelper.winInsertName.setText("")
    createMenu(List(MenuHelper.winText, MenuHelper.winInsertName,
      new CustomizedButton("OK", () => {
        val name: String = MenuHelper.winInsertName.text.value
        Highscores().addScore(name.length match {
          case 0 => "Anonymous"
          case e => if (e > 15) name.substring(0, 15)
          else name
        })
        createWinMenu2()
      })))
  }

  def createWinMenu2(): Unit = createMenu(List(
    MenuHelper.winText,
    new CustomizedButton("AGAIN?", () => {
      MenuHelper.resetNumOfMoves()
      loadLevel(1)
    }),
    new CustomizedButton("HIGH SCORES", () => createHghScoresMenu(createWinMenu2)),
    CustomizedButton.mainMenuButton))

  def createHghScoresMenu(function: () => Unit): Unit = createMenu(List(
    MenuHelper.highscores) ++ Highscores().getResults() ++
      List(new CustomizedButton("RETURN", function, _prefWidth = 150)))

  override def start(): Unit = {
    @tailrec
    def createHardcodedLevels(list: List[String], docName: String, num: Int = 1): Unit = {
      val writer = new PrintWriter(new File(docName + num + ".txt"))
      writer.write(list.head)
      writer.close()
      if (list.tail != Nil) createHardcodedLevels(list.tail, docName, num + 1)
    }

    val lvl1Training = "––––––––––––––\n––––––––––––––\n––––––––––––––\n––––––––––––––\n–ooo.–––––––––\n–oSoooo–––––––\n–ooooooooo––––\n––ooooooooo–––\n–––––––ooToo––\n–––––––ooo––––\n––––––––––––––\n––––––––––––––\n––––––––––––––\n––––––––––––––"
    val lvl2Training = "––––––––––––––\n––––––––––––––\n––––S–––––––––\n––––o–––––––––\n––––oo––––––––\n–ooooo––––––––\n–ooo.oo–––––––\n–ooo.ooooo––––\n––ooooooooo–––\n–––––––o.ooo––\n–––––––ooo–o––\n–––––––––T––––\n––––––––––––––\n––––––––––––––"
    val lvl3Training = "––––––––––––––\n––––––––––––––\n––––––––––––––\n–ooo––––––––––\n–oSo––––––––––\n–oo.......o–––\n–oo.......o–––\n–––––––––ooo––\n–––––––––Too––\n––––––––––––––\n––––––––––––––\n––––––––––––––\n––––––––––––––\n––––––––––––––"
    val lvl1 = "--------------\n--ooo---------\n--oSo---------\n--ooo---------\n---o----------\n---o----------\n---o----------\n---o----------\n--ooo-----ooo-\n--oooooooooTo-\n--ooo-----ooo-\n--------------\n--------------\n--------------"
    val lvl2 = "--------------\n---ooo--------\n---oSoo-------\n--ooooo.------\n--ooo.o.-o----\n---oooo.-ooo--\n----o.....ooo-\n-----.....ooo-\n------ooooooo-\n--------oooo--\n--------ooo---\n--------oTo---\n---------o----\n--------------"
    val lvl3 = "--------------\n---ooo--------\n---oSoo-------\n--ooooo-------\n--ooo.o.-o----\n---ooo..-ooo--\n----....--ooo-\n-----.....ooo-\n------..ooooo-\n--------oooo--\n--------ooo---\n--------oTo---\n---------o----\n--------------"
    val lvl4 = "––––––––––––––\n––––––––––––––\n––––––––––––––\n–ooo––––––––––\n–oSo––––––––––\n–oo––..o...–––\n–oooo..–..o–––\n–––––––––oo–––\n––––––––..T–––\n––––––––oooo––\n––––––––––––––\n––––––––––––––\n––––––––––––––\n––––––––––––––"
    val lvl5 = "--------------\n--------------\n--------------\n--------------\n-oo.o.--------\n-o.S....------\n-oo..o.oooo---\n--.o....oooo--\n--------...oo-\n--------ooo---\n----------To--\n---------ooo--\n--------------\n--------------"
    val lvl6 = "--------------\n--------------\n--------------\n--------------\n--S--oo-------\n--o--ooo------\n--oo-oooo-----\n--oooo-ooo----\n-------ooT----\n--------------\n--------------\n--------------\n--------------\n--------------"
    val lvl7 = "--------------\n--S--oo-------\n--o--ooo------\n--oo-oooo-----\n--oooo-ooo----\n-------ooo----\n--ooo----o----\n--ooT----o----\n---oo--ooo----\n---oooooo-----\n--------------\n--------------\n--------------\n--------------"
    if (!Files.exists(Paths.get("levelTraining1.txt")) || !Files.exists(Paths.get("level1.txt"))) {
      createHardcodedLevels(List(lvl1Training, lvl2Training, lvl3Training, lvl2), "levelTraining")
      createHardcodedLevels(List(lvl1, lvl2, lvl3, lvl4, lvl5, lvl6, lvl7), "level")
    }
    stage = new JFXApp3.PrimaryStage {
      title = "Bloxorz"
      resizable = false
    }
    Sounds.backgroundMusic.getSound.play()
    createMainMenu()
  }
}
