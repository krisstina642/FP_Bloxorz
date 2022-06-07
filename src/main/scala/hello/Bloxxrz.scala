package hello

import hello.Bloxxrz.stage
import javafx.scene.input.KeyCode
import jdk.internal.icu.util.CodePointTrie.ValueWidth
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
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.{DarkGray, DarkRed, Red, White}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.stage.Stage

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Bloxxrz extends JFXApp3 {

  val step=40
  val RED = new Color(255,0,0)
  val BLACK = new Color(0,0,0)
  val LIGHT_GREY = new Color(150,150,150)
  val BLUE = new Color(0, 0, 255)
  val GREY = new Color(100,100,100)
  var initialBlox:List[(Double, Double)]=null;
  var sceneWidth=600
  var sceneHeight=600


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
    but.text="LEVEL "+ int
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
      (e: ActionEvent) => startLevel(stage, int)
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

  case class getLinesFromFile(src:String){
    def content:List[String]={
      val bufferedSource = Source.fromFile(src)
      val str:List[String] = bufferedSource.getLines.toList
      bufferedSource.close
      System.out.println(str)
      str
    }
  }

  def getRectangles(src: List[String]):List[Rectangle]={ // all rectangles, empty space, special blocks, end_point
    val rec = new ListBuffer[Rectangle]()
    for ((line:String, i) <- src.zipWithIndex)
      for ((e,j) <- line.toUpperCase.zipWithIndex) {
        e.toUpper match {
          case '–'  => rec += createRect( i*step, j*step , BLACK)
          case '-' => rec += createRect( i*step, j*step , BLACK)
          case 'O' => rec += createRect( i*step, j*step , GREY)
          case 'S' => rec += createRect(i*step, j*step , GREY)
          case 'T' => rec += createRect( i*step, j*step , RED)
          case '.' => rec += createRect( i*step, j*step , LIGHT_GREY)
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
          case '–' => empty.append((i*step, j*step))
          case '-' => empty.append((i*step, j*step))
          case 'S' => initialBlox.append((i*step, j*step)).append((i*step, j*step))
          case 'T' => endPos = (i*step, j*step)
          case '.' => special.append((i*step, j*step))
          case _ => {}
        }
    }
    }
    (initialBlox.toList, endPos, empty.toList,special.toList)
  }

  case class State(stage: Stage,level: Int, content:List[String], blox:List[(Double, Double)], endPos:(Double,Double), emptyBloxList:List[(Double, Double)], specialBloxList:List[(Double, Double)]){

    def newState(dir: Int): State={
      val (x1, y1) = blox.head
      val (x2, y2) = blox.tail.head
      val (newx1, newy1, newx2, newy2) = dir match {
        case 1 if x1==x2 && y1==y2 => (x1, y1-step, x2, y2-2*step)
        case 1 if x1!=x2 && y1==y2 => (x1, y1-step, x2, y2-step)
        case 1 if x1==x2 && y1<y2 => (x1, y1-step, x2, y2-2*step)
        case 1 if x1==x2 && y2<y1 => (x1, y1-2*step, x2, y2-step)

        case 2 if x1==x2 && y1==y2 => (x1, y1+step, x2, y2+2*step)
        case 2 if x1!=x2 && y1==y2 => (x1, y1+step, x2, y2+step)
        case 2 if x1==x2 && y1<y2 => (x1, y1+2*step, x2, y2+step)
        case 2 if x1==x2 && y2<y1 => (x1, y1+step, x2, y2+2*step)

        case 3 if x1==x2 && y1==y2 => (x1-step, y1, x2-2*step, y2)
        case 3 if x1<x2 && y1==y2 => (x1-step, y1, x2-2*step, y2)
        case 3 if x1>x2 && y1==y2 => (x1-2*step, y1, x2-step, y2)
        case 3 if x1==x2 && y2!=y1 => (x1-step, y1, x2-step, y2)

        case 4 if x1==x2 && y1==y2 => (x1+step, y1, x2+2*step, y2)
        case 4 if x1<x2 && y1==y2 => (x1+2*step, y1, x2+step, y2)
        case 4 if x1>x2 && y1==y2 => (x1+step, y1, x2+2*step, y2)
        case 4 if x1==x2 && y2!=y1 => (x1+step, y1, x2+step, y2)

        case _ =>(x1, y1, x2, y2)
      }

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
      getRectangles(content) ::: blox.map { case (x, y) => createRect(x, y, BLUE) }
    }
  }

  def chooseLevel(stage: Stage)={
    var stop=4;
    val vBox=new VBox()
    var hBox=new HBox()
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

  def startLevel(stage: Stage, int: Int): Unit ={
    val game_settings= getLinesFromFile("level"+int+".txt")
    val arena=game_settings.content
    getRectangles(arena)
    val (blox, end, empty, spec)=getBlocks(arena)
    initialBlox = blox
    val state=  ObjectProperty(State(stage, int, arena,blox, end, empty, spec))
    stage.scene.value.content = state.value.rectangles
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
    stage.title = "Bloxxrz"
    val cnt=new VBox{
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
    createMenu(stage, List(start,quit))
  }
  def createPauseMenu(stage: Stage, level:Int, win: Boolean): Unit ={
    val again=createButton("AGAIN?")
    val main=createButton("MAIN MENU")
    again.onAction = (e:ActionEvent) => {startLevel(stage, level)}
    main.onAction = (e:ActionEvent) => {createMainMenu(stage)}
    val cnt:VBox=createMenu(stage, List(again,main))
    cnt.getChildren.add(0,new Text {
      text = win match{
        case true => "YOU WIN"
        case false => "FAIL"
      }
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
