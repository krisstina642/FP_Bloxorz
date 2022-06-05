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
import scalafx.scene.layout.VBox
import scalafx.scene.shape.Rectangle
import scalafx.stage.Stage

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

  def createRect(xr:Double, yr:Double, color: Color) = new Rectangle{
    x=xr
    y=yr
    width = step
    height = step
    fill = color
  }

  case class GameSettings(src:String){
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

  case class State(content:List[String], blox:List[(Double, Double)], endPos:(Double,Double), emptyBloxList:List[(Double, Double)], specialBloxList:List[(Double, Double)]){

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
          initialBlox
        } /// end game
        else if (newx1==newx2 && (newx2==endPos._1) && newy1==newy2 && newy2==endPos._2 ){
          System.out.println("win")
          initialBlox //win
        }
        else {
           List((newx1, newy1), (newx2, newy2))
        }
      State(content, newBlox, endPos, emptyBloxList, specialBloxList)
    }
    def rectangles: List[Rectangle]= {
      getRectangles(content) ::: blox.map { case (x, y) => createRect(x, y, BLUE) }
    }
  }

  def endGame(boolean: Boolean):VBox= {
    if (boolean)  new VBox {
      prefWidth = 600
      prefHeight = 600
      padding = Insets(50, 70, 70, 50)
      alignment = Pos.BaselineCenter
      alignmentInParent = Pos.BaselineCenter
      children = Seq(new Button{
        text= "Quit"
        prefWidth = 200.0
        prefHeight = 30.0
      })
    }
    else new VBox {
      prefWidth = 600
      prefHeight = 600
      padding = Insets(50, 70, 70, 50)
      alignment = Pos.BaselineCenter
      alignmentInParent = Pos.BaselineCenter
      children = Seq(new Button{
        text= "Again"
        prefWidth = 200.0
        prefHeight = 30.0
      })
    }
  }

  def startLevel(stage: Stage): Unit ={
    val game_settings= ObjectProperty(GameSettings("example.txt"))
    val arena=game_settings.value.content
    getRectangles(arena)
    val (blox, end, empty, spec)=getBlocks(arena);
    initialBlox = blox
    val state=  ObjectProperty(State(arena,blox, end, empty, spec))
    stage.scene.value.content = state.value.rectangles
    var direct = 1;
    stage.scene.value.onKeyPressed = key => {

      state.update(state.value.newState(
        key.getCode match {
        case KeyCode.W => 1
        case KeyCode.S => 2
        case KeyCode.A =>  3
        case KeyCode.D =>  4
        case KeyCode.UP =>  1
        case KeyCode.DOWN =>  2
        case KeyCode.LEFT =>  3
        case KeyCode.RIGHT =>  4
        case _ => System.out.println(" WRONG KEY ")
          -1
      }))
      stage.scene.value.content = state.value.rectangles
    }
  }

  override def start(): Unit = {

    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      //    initStyle(StageStyle.Unified)

      val button_start= new Button{
        text= "Start"
        prefWidth = 200.0
        prefHeight = 30.0
        onAction = (e: ActionEvent) => startLevel(stage)
      }

      val button_quit= new Button{
        text= "Quit"
        prefWidth = 200.0
        prefHeight = 30.0
        onAction = (e: ActionEvent) => stage.close()
      }

      title = "Bloxxrz"
      scene = new Scene {
        fill = Color.rgb(38, 38, 38)
     /*   content = state.value.rectangles
        var direct = 1;
        onKeyPressed = key => {
          key.getCode match {
          case KeyCode.W => direct = 1
          case KeyCode.S => direct = 2
          case KeyCode.A => direct = 3
          case KeyCode.D => direct = 4
          case KeyCode.UP => direct = 1
          case KeyCode.DOWN => direct = 2
          case KeyCode.LEFT => direct = 3
          case KeyCode.RIGHT => direct = 4
          case _ => System.out.println(" WRONG KEY ")
        }
          state.update(state.value.newState(direct))
          content = state.value.rectangles
        }*/
       content = new VBox{
          prefWidth = 600
          prefHeight = 600
          padding = Insets(50, 70, 70, 50)
          alignment = Pos.BaselineCenter
          alignmentInParent= Pos.BaselineCenter
          children=Seq(button_start, button_quit)
        }
      /*  val hbox = new HBox {
          padding = Insets(50, 80, 50, 80)
          children = Seq(
            new Text {
              text = " YOU WIN!"
              style = "-fx-font: normal bold 100pt sans-serif"
              fill = new LinearGradient(
                endX = 0,
                stops = Stops(Red, DarkRed))
            },
            new Text {
              text = "FAIL!"
              style = "-fx-font: italic bold 100pt sans-serif"
              fill = new LinearGradient(
                endX = 0,
                stops = Stops(White, DarkGray)
              )
              effect = new DropShadow {
                color = DarkGray
                radius = 15
                spread = 0.25
              }
            }
          )
        } */
       // val but = new Button("Start");

      //  content=List(but);
      }
    }
  }
}
