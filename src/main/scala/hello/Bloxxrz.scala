package hello

import scalafx.application.JFXApp3
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.paint._
import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.scene.shape.Rectangle


object Bloxxrz extends JFXApp3 {

  val initialBlox:List[(Double,Double)]=List(
    (200,200),
    (200,200)
  )

  case class State(blox:List[(Double, Double)], endPos:(Double, Double)){
    def newState(dir: Int): State={
      System.out.println(dir)
      val (x1, y1) = blox.head
      val (x2, y2) = blox.tail.head
      val (newx1, newy1, newx2, newy2) = dir match {
        case 1 if x1==x2 && y1==y2 => (x1, y1-25, x2, y2-50)
        case 1 if x1!=x2 && y1==y2 => (x1, y1-25, x2, y2-25)
        case 1 if x1==x2 && y1<y2 => (x1, y1-25, x2, y2-50)
        case 1 if x1==x2 && y2<y1 => (x1, y1-50, x2, y2-25)

        case 2 if x1==x2 && y1==y2 => (x1, y1+25, x2, y2+50)
        case 2 if x1!=x2 && y1==y2 => (x1, y1+25, x2, y2+25)
        case 2 if x1==x2 && y1<y2 => (x1, y1+50, x2, y2+25)
        case 2 if x1==x2 && y2<y1 => (x1, y1+25, x2, y2+50)

        case 3 if x1==x2 && y1==y2 => (x1-25, y1, x2-50, y2)
        case 3 if x1<x2 && y1==y2 => (x1-25, y1, x2-50, y2)
        case 3 if x1>x2 && y1==y2 => (x1-50, y1, x2-25, y2)
        case 3 if x1==x2 && y2!=y1 => (x1-25, y1, x2-25, y2)

        case 4 if x1==x2 && y1==y2 => (x1+25, y1, x2+50, y2)
        case 4 if x1<x2 && y1==y2 => (x1+50, y1, x2+25, y2)
        case 4 if x1>x2 && y1==y2 => (x1+25, y1, x2+50, y2)
        case 4 if x1==x2 && y2!=y1 => (x1+25, y1, x2+25, y2)

        case _ =>(x1, y1, x2, y2)
      }

      val newBlox :List[(Double,Double)]=
        if (newx1 < 0 || newx1 > 600 || newy1 < 0 || newy1 > 600 ||
          newx2 < 0 || newx2 > 600 || newy2 < 0 || newy2 > 600)
          initialBlox /// end game
        else if (newx1==newx2==endPos._1 && newy1==newy2==endPos._2 ){
          initialBlox //win
        }
        else {
           List((newx1, newy1), (newx2, newy2))
        }
        State(newBlox, endPos)
    }

    def rectangles: List[Rectangle]= rect(endPos._1, endPos._2, Color.rgb(100, 100, 100)):: blox.map{
      case(x,y) => rect (x,y, Color.rgb(200, 200, 200))
    }
  }

  def rect(xr:Double, yr:Double, color: Color) = new Rectangle{
    x=xr;
    y=yr;
    width = 25
    height = 25
    fill = color
  }

  override def start(): Unit = {

    val state=  ObjectProperty(State(initialBlox, (500,500)))
    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      //    initStyle(StageStyle.Unified)

      val button_start= new Button{
        text= "Start"
        prefWidth = 200.0
        prefHeight = 30.0
      }
      button_start.onAction = (e: ActionEvent) =>{
        stage.close()
      }

      val button_quit= new Button{
        text= "Quit"
        prefWidth = 200.0
        prefHeight = 30.0
      }
      button_quit.onAction = (e: ActionEvent) =>{
        stage.close()
      }
      title = "Bloxxrz"
      scene = new Scene {
        fill = Color.rgb(38, 38, 38)
        content = state.value.rectangles
        var direct = 1;
        onKeyPressed = key => {key.getText match {
          case "w" => direct = 1
          case "s" => direct = 2
          case "a" => direct = 3
          case "d" => direct = 4
        }
          state.update(state.value.newState(direct))
          content = state.value.rectangles
        }
      /*  content = new VBox{
          prefWidth = 600
          prefHeight = 600
          padding = Insets(50, 70, 70, 50)
          alignment = Pos.BaselineCenter
          alignmentInParent= Pos.BaselineCenter
          children=Seq(button_start, button_quit)
        }*/
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
