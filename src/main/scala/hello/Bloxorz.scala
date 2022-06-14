package hello


import javafx.scene.input.KeyCode
import scalafx.application.JFXApp3
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.paint._
import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.effect.DropShadow
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.VBox.getMargin
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.{DarkGray, DarkRed, Red, White}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.stage.FileChooser.ExtensionFilter
import scalafx.stage.{FileChooser, Stage}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Bloxorz extends JFXApp3 {

  val step = 40
  var initialBlox:List[(Double, Double)] = null;
  var sceneWidth = 600
  var sceneHeight = 600


  def createButton(text_ :String, function: Unit):Button ={
    new Button {
      text = text_
      prefWidth = 200.0
      prefHeight = 30.0
      onAction = (e: ActionEvent) => {
        function
      }
    }
  }
  def createMapButton(width: Double, height:Double, int:Int): Button ={
    val but= new Button()
    but.graphic = new ImageView() {image = new Image("level"+int+".png")}
    but.setPrefSize(width,height)
      but.alignmentInParent =
      int%4 match {
        case 1 =>Pos.TopLeft
        case 2 =>Pos.TopRight
        case 3 =>Pos.BottomLeft
        case 0 =>Pos.BottomRight
      }
     but.onAction = {
          System.out.println("action")
      (e: ActionEvent) => loadLevel(stage, int, false)
    }
        but
  }
  def createRect(xr:Double, yr:Double, color: Color) = new Rectangle{
    x=xr
    y=yr
    width = step
    height = step
    fill = color
  }

  def getLinesFromFile(src:String):List[String] = {
      val bufferedSource = Source.fromFile(src)
      val str:List[String] = bufferedSource.getLines.toList
      bufferedSource.close
      str
  }
  def getRectangles(src: List[String]):List[Rectangle]={ // all rectangles, empty space, special blocks, end_point
    val rec = new ListBuffer[Rectangle]()
    for ((line:String, i) <- src.zipWithIndex)
      for ((e,j) <- line.toUpperCase.zipWithIndex) {
        e.toUpper match {
          case '–'  => rec += createRect( j*step, i*step , Color.Black)
          case '-' => rec += createRect( j*step, i*step , Color.Black)
          case 'O' => rec += createRect( j*step, i*step , Color.Gray)
          case 'S' => rec += createRect(j*step, i*step , Color.Gray)
          case 'T' => rec += createRect( j*step, i*step , Color.Red)
          case '.' => rec += createRect( j*step, i*step , Color.LightGray)
        }
    }
    rec.toList
  }
  def getBlocks(src: List[String]):(List[(Double,Double)],(Double,Double),List[(Double,Double)], List[(Double,Double)])={ // init, end, empty space, special blocks, end_point
    val empty = ListBuffer[(Double,Double)]()
    val special = ListBuffer[(Double,Double)]()
    val initialBlox = ListBuffer[(Double,Double)]()
    var endPos:(Double, Double)=(80,80)
    for ((line:String, i) <- src.zipWithIndex) {
    for ((e,j) <- line.toUpperCase.zipWithIndex) {
        e.toUpper match {
          case '–' => empty.append((j*step, i*step))
          case '-' => empty.append((j*step, i*step))
          case 'S' => initialBlox.append((j*step, i*step)).append((j*step, i*step))
          case 'T' => endPos = (j*step, i*step)
          case '.' => special.append((j*step, i*step))
          case _ => {}
        }
    }
    }
    (initialBlox.toList, endPos, empty.toList,special.toList)
  }

  case class State(stage: Stage,level: Int, content:List[String], blox:List[(Double, Double)], endPos:(Double,Double), emptyBloxList:List[(Double, Double)], specialBloxList:List[(Double, Double)]){


    def newState(dir: Int): State={
      val (newx1, newy1, newx2, newy2) = calculatePosition(blox.head._1,blox.head._2,blox.tail.head._1,blox.tail.head._2, dir)

      val newBlox :List[(Double,Double)]=
        if (newx1 < 0 || newx1 >= 600 || newy1 < 0 || newy1 >= 600 ||
          newx2 < 0 || newx2 >= 600 || newy2 < 0 || newy2 >= 600 ||
          (newx1==newx2 && newy1==newy2 && specialBloxList.contains((newx1,newy1))) ||
          emptyBloxList.contains((newx1,newy1)) || emptyBloxList.contains((newx2,newy2))) {
          System.out.println("end game")
          createPauseMenu(stage,level, false)
          null
        } /// end game
        else if (newx1==newx2 && (newx2==endPos._1) && newy1==newy2 && newy2==endPos._2 ){
          System.out.println("win")
          createPauseMenu(stage,level,true)
          null
        }
        else {
           List((newx1, newy1), (newx2, newy2))
        }
      State(stage, level, content, newBlox, endPos, emptyBloxList, specialBloxList)

    }
    def rectangles: List[Rectangle]= {
      getRectangles(content) ::: blox.map { case (x, y) => createRect(x, y, Color.Blue) }
    }
  }

  def chooseLevel(stage: Stage)={
    val stop=4;
    val vBox=new VBox(20)
    var hBox=new HBox(20)

    hBox.alignment = Pos.BaselineCenter

    @tailrec
    def chlvl(int: Int) {
      if (int<=stop && Files.exists(Paths.get("level"+int+".txt"))) {
        hBox.getChildren().add(createMapButton(sceneWidth/3, sceneHeight/3,int))

        if(hBox.getChildren().size()==2) {
          vBox.getChildren().add(hBox)
          hBox=new HBox()
        }
        chlvl(int+1)
      }
    }
    chlvl(1)
    if (hBox.getChildren().size>0) vBox.getChildren().add(hBox)
    stage.scene.value.content = vBox
  }
  def findSolution(stage:Stage, level:Int)={
    val fileChooser: FileChooser = new FileChooser
    fileChooser.setTitle("Save As")
    fileChooser.getExtensionFilters().add(new ExtensionFilter("Text","*.txt"))
    val saveFile = fileChooser.showSaveDialog(stage)
    if (saveFile!=null) {

      val writer = new PrintWriter(new File(saveFile.getAbsolutePath))
      val arena = getLinesFromFile("level" + level + ".txt")
      val (blox, end, empty, spec) = getBlocks(arena) ////////////

      def nextPosition(state:(Double, Double, Double, Double)):List[(Double,Double,Double,Double)] ={

        val acc:ListBuffer[(Double,Double,Double, Double)]= ListBuffer[(Double,Double,Double, Double)]()
        for(dir<-1 to 4) {
          val (newx1, newy1, newx2, newy2) = calculatePosition(state._1, state._2,state._3, state._4, dir)
          if (newx1>=0 && newx2>=0 && newy1>=0 && newy2>=0 && !(empty.contains((newx1,newy1)) || empty.contains((newx2,newy2))) &&
            !(spec.contains((newx1,newy1)) && (newx1,newy1)==(newx2,newy2))){
            acc += ((newx1, newy1, newx2, newy2))
          }
        }
        acc.toList
      }

      @tailrec
      def writeToFile(current:Int, state:List[(Double,Double,Double,Double)], accFrom:List[Int]):Unit={
        if(current==0) writer.close()
        else{
          if (state(accFrom(current))._1 < state(current)._1) writer.write("l\n")
          else if (state(accFrom(current))._1 > state(current)._1) writer.write("r\n")
          else if (state(accFrom(current))._2 < state(current)._2) writer.write("u\n")
          else if (state(accFrom(current))._2 > state(current)._2) writer.write("d\n")
          writeToFile(accFrom(current), state, accFrom)
        }
      }

      @tailrec
      def solveLevel( current:Int, state:List[(Double,Double,Double,Double)], accFrom:List[Int], end :(Double, Double)): Unit ={
        if (state.length<current+1) {
          //System.err.println("No Solutions")
          writer.write("No Solutions")
          writer.close()
        }
        else if (state(current)._1==state(current)._3 && state(current)._2==state(current)._4 && state(current)._1==end._1 && state(current)._2==end._2) {
          //System.out.println("solved")
          writeToFile(current, state, accFrom)
        }
        else {
          val nxt=nextPosition(state(current))
          solveLevel(current+1, state ::: nxt, accFrom ::: List.fill(nxt.length)(current), end)
        }
      }
      solveLevel(0, List((end._1, end._2, end._1, end._2)), List(0), blox.head)
    }
  }
  def calculatePosition(x1:Double,y1:Double,x2:Double,y2:Double, direction:Int):(Double, Double, Double, Double)={
    direction match {
      case 1 if x1 == x2 && y1 == y2 => (x1, y1 - step, x2, y2 - 2 * step)
      case 1 if x1 != x2 && y1 == y2 => (x1, y1 - step, x2, y2 - step)
      case 1 if x1 == x2 && y1 < y2 => (x1, y1 - step, x2, y2 - 2 * step)
      case 1 if x1 == x2 && y2 < y1 => (x1, y1 - 2 * step, x2, y2 - step)

      case 2 if x1 == x2 && y1 == y2 => (x1, y1 + step, x2, y2 + 2 * step)
      case 2 if x1 != x2 && y1 == y2 => (x1, y1 + step, x2, y2 + step)
      case 2 if x1 == x2 && y1 < y2 => (x1, y1 + 2 * step, x2, y2 + step)
      case 2 if x1 == x2 && y2 < y1 => (x1, y1 + step, x2, y2 + 2 * step)

      case 3 if x1 == x2 && y1 == y2 => (x1 - step, y1, x2 - 2 * step, y2)
      case 3 if x1 < x2 && y1 == y2 => (x1 - step, y1, x2 - 2 * step, y2)
      case 3 if x1 > x2 && y1 == y2 => (x1 - 2 * step, y1, x2 - step, y2)
      case 3 if x1 == x2 && y2 != y1 => (x1 - step, y1, x2 - step, y2)

      case 4 if x1 == x2 && y1 == y2 => (x1 + step, y1, x2 + 2 * step, y2)
      case 4 if x1 < x2 && y1 == y2 => (x1 + 2 * step, y1, x2 + step, y2)
      case 4 if x1 > x2 && y1 == y2 => (x1 + step, y1, x2 + 2 * step, y2)
      case 4 if x1 == x2 && y2 != y1 => (x1 + step, y1, x2 + step, y2)

      case _ => (x1, y1, x2, y2)
    }
  }


  def loadLevel(stage: Stage, level: Int, fromFile:Boolean): Unit ={
    val arena=getLinesFromFile("level"+level+".txt")
    val (blox, end, empty, spec)=getBlocks(arena)
    initialBlox = blox
    val state = ObjectProperty(State(stage, level, arena, blox, end, empty, spec))
    stage.scene.value.content = state.value.rectangles

    def playFromFile(): Unit ={
      val fileChooser: FileChooser = new FileChooser
      fileChooser.getExtensionFilters().add(new ExtensionFilter("Text","*.txt"))
      val selectedFile = fileChooser.showOpenDialog(stage)
      if (selectedFile != null) {
        val sequence = getLinesFromFile(selectedFile.getAbsolutePath)
        for (line: String <- sequence) {
          if (state.value.blox != null)
            state.update(
              state.value.newState(
                line.toUpperCase() match {
                  case "U" => 1
                  case "D" => 2
                  case "L" => 3
                  case "R" => 4
                  case _ => startLevel(stage, state)
                    0
                }))
          if (state.value.blox != null) stage.scene.value.content = state.value.rectangles
        }
      }
    }

    if (fromFile) playFromFile()
    startLevel(stage, state)
  }
  def startLevel(stage: Stage, state:ObjectProperty[State]): Unit ={
    stage.scene.value.onKeyPressed = key => {
     if (List(KeyCode.W,KeyCode.S,KeyCode.A,KeyCode.D,KeyCode.UP,KeyCode.DOWN,KeyCode.LEFT,KeyCode.RIGHT).contains(key.getCode) && state.value.blox!=null)
     {
      state.update(
        state.value.newState(
        key.getCode match {
        case KeyCode.W => 1
        case KeyCode.S => 2
        case KeyCode.A =>  3
        case KeyCode.D =>  4
        case KeyCode.UP =>  1
        case KeyCode.DOWN =>  2
        case KeyCode.LEFT =>  3
        case KeyCode.RIGHT =>  4
      }))
     if(state.value.blox!=null)  stage.scene.value.content = state.value.rectangles
    }
    }
  }

  def createMenu(stage: Stage, list: List[Button]): VBox={
    stage.title = "Bloxorz"
    val cnt=new VBox{
      spacing = 5
      prefWidth = 600
      prefHeight = 600
      padding = Insets(50, 70, 70, 50)
      alignment = Pos.BaselineCenter
      alignmentInParent= Pos.BaselineCenter
      children=list
    }
    stage.scene = new Scene {
      fill = Color.rgb(38, 38, 38)
      content = cnt
    }
    cnt
  }
  def createButton(_text:String): Button ={
    new Button{
      text= _text
      prefWidth = 200.0
      prefHeight = 30.0
    }
  }
  def createMainMenu(stage: Stage)={
    val quit=createButton("QUIT")
    val start=createButton("START")
    quit.onAction = (e: ActionEvent) => stage.close()
    start.onAction = (e: ActionEvent) => chooseLevel(stage)
    val cnt:VBox=createMenu(stage, List(start,quit))
    cnt.getChildren.add(0,new Text {
      text = "BLOXORZ"
      margin = Insets(50, 50, 50, 50)
      style = "-fx-font: bold 35pt sans-serif"
      fill = new LinearGradient(
        endX = 0,
        stops = Stops(Red, DarkGray))
      effect = new DropShadow {
        color = DarkRed
        radius = 15
        spread = 0.25
        }
    })
  }

  def createPauseMenu(stage: Stage, level:Int, win: Boolean): Unit ={
    val again=createButton("AGAIN?")
    val solution=createButton("SOLUTION")
    val sequence=createButton("PLAY FROM FILE")
    val main=createButton("MAIN MENU")
    solution.onAction = (e:ActionEvent) => {findSolution(stage, level)}
    sequence.onAction = (e:ActionEvent) => {loadLevel(stage, level,true)}
    again.onAction = (e:ActionEvent) => {loadLevel(stage, level,false)}
    main.onAction = (e:ActionEvent) => {createMainMenu(stage)}
    val cnt:VBox=createMenu(stage, List(again,solution,sequence,main))
    cnt.getChildren.add(0,new Text {
      text = win match{
        case true => "YOU WIN"
        case false => "FAIL"
      }
      margin = Insets(50, 50, 50, 50)
      style = "-fx-font: bold 20pt sans-serif"
      fill = new LinearGradient(
        endX = 0,
        stops = Stops(win match{
          case true => White
          case false => Red
        }, DarkGray)
      )
      effect = new DropShadow {
        color = win match{
          case true => DarkGray
          case false => DarkRed
        }
        radius = 15
        spread = 0.25
      }}
    )
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      }
    createMainMenu(stage)
    }

}