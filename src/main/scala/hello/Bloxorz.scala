package hello

import javafx.scene.input.KeyCode
import scalafx.application.JFXApp3
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ContextMenu, MenuItem}
import scalafx.scene.paint._
import scalafx.Includes._
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.effect.DropShadow
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.MouseEvent._
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
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
  var initialBlox:List[(Double, Double)] = null
  var sceneWidth = 600
  var sceneHeight = 600

  def getScalaFill(paint: Paint):Paint=paint

  val specColor: Paint =getScalaFill(Color.LightGray)
  val basicColor: Paint=getScalaFill(Color.Gray)
  val emptyColor: Paint=getScalaFill(Color.Black)
  val startColor: Paint=getScalaFill(Color.Blue)
  val endColor: Paint=getScalaFill(Color.Red)
  
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

  def createRect(xr:Double, yr:Double, color: Paint): Rectangle = new Rectangle{
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
          case '–'  => rec += createRect( j*step, i*step , emptyColor)
          case '-' => rec += createRect( j*step, i*step , emptyColor)
          case 'O' => rec += createRect( j*step, i*step , basicColor)
          case 'S' => rec += createRect(j*step, i*step , basicColor)
          case 'T' => rec += createRect( j*step, i*step , endColor)
          case '.' => rec += createRect( j*step, i*step , specColor)
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
      for ((e, j) <- line.toUpperCase.zipWithIndex) {
        e.toUpper match {
          case '–' => empty.append((j * step, i * step))
          case '-' => empty.append((j * step, i * step))
          case 'S' => initialBlox.append((j * step, i * step)).append((j * step, i * step))
          case 'T' => endPos = (j * step, i * step)
          case '.' => special.append((j * step, i * step))
          case _ =>
        }
      }
    }
    (initialBlox.toList, endPos, empty.toList,special.toList)
  }

  case class State(stage: Stage, src:String, content:List[String], blox:List[(Double, Double)], endPos:(Double,Double), emptyBloxList:List[(Double, Double)], specialBloxList:List[(Double, Double)]){

    def createPauseMenu(win: Boolean): Unit ={
      val again=createButton("AGAIN?")
      val solution=createButton("SOLUTION")
      val sequence=createButton("PLAY FROM FILE")
      val main=createButton("MAIN MENU")
      solution.onAction = (e:ActionEvent) => {findSolution(src)}
      sequence.onAction = (e:ActionEvent) => {loadLevel(src, fromFile = true)}
      again.onAction = (e:ActionEvent) => {loadLevel(src,fromFile = false)}
      main.onAction = (e:ActionEvent) => {createMainMenu()}
      val cnt:VBox=createMenu(List(again,solution,sequence,main))
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
            case true => Color.White
            case false => Color.Red
          }, Color.DarkGray)
        )
        effect = new DropShadow {
          color = win match{
            case true => Color.DarkGray
            case false => Color.DarkRed
          }
          radius = 15
          spread = 0.25
        }}
      )
    }

    def newState(dir: Int): State={
      val (newx1, newy1, newx2, newy2) = calculatePosition(blox.head._1,blox.head._2,blox.tail.head._1,blox.tail.head._2, dir)

      val newBlox :List[(Double,Double)]=
        if (newx1 < 0 || newx1 >= 600 || newy1 < 0 || newy1 >= 600 ||
          newx2 < 0 || newx2 >= 600 || newy2 < 0 || newy2 >= 600 ||
          (newx1==newx2 && newy1==newy2 && specialBloxList.contains((newx1,newy1))) ||
          emptyBloxList.contains((newx1,newy1)) || emptyBloxList.contains((newx2,newy2))) {
          System.out.println("end game")
          createPauseMenu( false)
          null
        } /// end game
        else if (newx1==newx2 && (newx2==endPos._1) && newy1==newy2 && newy2==endPos._2 ){
          System.out.println("win")
          createPauseMenu(true)
          null
        }
        else {
           List((newx1, newy1), (newx2, newy2))
        }
      State(stage, src, content, newBlox, endPos, emptyBloxList, specialBloxList)
    }
    def rectangles: List[Rectangle]= {
      getRectangles(content) ::: blox.map { case (x, y) => createRect(x, y, startColor) }
    }
  }

  def chooseLevel(play: Boolean)={  // true -play false - edit
    val stop=4
    val vBox=new VBox(20)
    var hBox=new HBox(20)

    hBox.alignment = Pos.BaselineCenter

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
        (e: ActionEvent) => play match {
          case true => loadLevel("level"+int+".txt", false)
          case _ => createLevel(int)
        }
      }
      but
    }

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
    if (hBox.getChildren.size>0) vBox.getChildren.add(hBox)
    stage.scene.value.content = vBox
  }

  def saveAs(): File ={
    val fileChooser: FileChooser = new FileChooser
    fileChooser.setTitle("Save As")
    fileChooser.getExtensionFilters.add(new ExtensionFilter("Text","*.txt"))
    fileChooser.showSaveDialog(stage)
  }

  def open(): File ={
    val fileChooser: FileChooser = new FileChooser
    fileChooser.getExtensionFilters.add(new ExtensionFilter("Text","*.txt"))
    fileChooser.showOpenDialog(stage)
  }

  def findSolution(src:String): Unit ={
    val saveFile = saveAs()
    if (saveFile!=null) {

      val writer = new PrintWriter(new File(saveFile.getAbsolutePath))
      val arena = getLinesFromFile(src)
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
          val nxt=nextPosition(state(current)).filterNot(p=>state.contains(p))
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

  def createLevel(level: Int): Unit = {
    val arena=getLinesFromFile("level"+level+".txt")
    val (blox, end, empty, spec)=getBlocks(arena)
    val rectangles:List[Rectangle]= getRectangles(arena)
    rectangles.foreach(r=>if (r.x.value ==blox.head._1 && r.y.value ==blox.head._2) r.setFill(startColor))
    stage.scene.value.content=rectangles
    def col(rectangle: Rectangle, color: Paint): Unit ={
      rectangles.foreach(r=>{if(getScalaFill(r.getFill) == getScalaFill(color)) r.setFill(basicColor)})
      rectangle.setFill(color)
      }
    def invertMap(): Unit ={
      rectangles.foreach(r=>{if(getScalaFill(r.getFill) == getScalaFill(endColor)) r.setFill(startColor)
      else if(getScalaFill(r.getFill) == getScalaFill(startColor)) r.setFill(endColor) })
    }
    def replaceSpecial(): Unit ={
      rectangles.foreach(r=>{if(getScalaFill(r.getFill) == getScalaFill(specColor)) r.setFill(basicColor)})
    }
      def isOnEdge(num: Int, color: Paint): Boolean = {
        if (num < 14 * 13 && getScalaFill(rectangles(num + 14).getFill) == getScalaFill(color)) {
          System.out.println("down")
          return true
        }
        if (num % 14 < 13 && getScalaFill(rectangles(num + 1).getFill) == getScalaFill(color)) return true
        if (num % 14 > 0 && getScalaFill(rectangles(num - 1).getFill) == getScalaFill(color)) return true
        if (num > 13 && getScalaFill(rectangles(num - 14).getFill) == getScalaFill(color)) return true
        false
      }


    rectangles.indices.foreach(i=>{
     //r.onMouseDragOver = (e: MouseEvent) => System.out.println("drag")
      val r= rectangles(i)
      r.handleEvent(MouseEntered){
        a:MouseEvent=>{
          r.setArcWidth(20)
          r.setArcHeight(20)
        }
      }
      r.handleEvent(MouseExited){
        a:MouseEvent=>{
          r.setArcWidth(0)
          r.setArcHeight(0)
        }
      }

      val deleteBasic:MenuItem = new MenuItem("Delete")
      deleteBasic.onAction=(e: ActionEvent) =>{r.setFill(emptyColor)}
      val addBasic:MenuItem = new MenuItem("Add")
      addBasic.onAction=(e: ActionEvent) =>{r.setFill(basicColor)}
      val special:MenuItem = new MenuItem("Set Special")
      special.onAction=(e: ActionEvent) =>{r.setFill(specColor)}
      val basic:MenuItem = new MenuItem("Set Basic")
      basic.onAction=(e: ActionEvent) =>{r.setFill(basicColor)}
      val start:MenuItem = new MenuItem("Set Start")
      start.onAction=(e: ActionEvent) =>{col(r, startColor);}
      val end:MenuItem = new MenuItem("Set End")
      end.onAction=(e: ActionEvent) =>{col(r, endColor);}
      val invert:MenuItem = new MenuItem("Invert")
      invert.onAction=(e: ActionEvent) =>{invertMap()}
      val removeSpec:MenuItem = new MenuItem("Remove Special")
      removeSpec.onAction=(e: ActionEvent) =>{replaceSpecial()}
      val saveMapAs:MenuItem = new MenuItem("Save As")
      saveMapAs.onAction=(e: ActionEvent) =>{
        val saveFile = saveAs()
        if (saveFile!=null) {
          System.out.println("NOT NULL " + rectangles.length)
          val writer = new PrintWriter(new File(saveFile.getAbsolutePath))
          @tailrec
          def writeMap(current:Int): Unit ={
            if (current==14*14) writer.close()
            else{
              if (getScalaFill(rectangles(current).getFill())==specColor) writer.write(".")
              else if (getScalaFill(rectangles(current).getFill())==basicColor) writer.write("o")
              else if (getScalaFill(rectangles(current).getFill())==endColor) writer.write("T")
              else if (getScalaFill(rectangles(current).getFill())==startColor) writer.write("S")
              else if (getScalaFill(rectangles(current).getFill())==emptyColor) writer.write("-")
              if (current % 14 == 13 && current!=rectangles.length-1) writer.write("\n")
              writeMap(current+1)
            }
          }

          writeMap(0)
        }
      }

      r.handleEvent(MousePressed){
        a:MouseEvent=>{
          val contextMenu: ContextMenu = new ContextMenu()
          if (a.secondaryButtonDown){
            contextMenu.getItems.addAll(invert,removeSpec,saveMapAs)
          }
          else {
            r.setArcWidth(80)
            r.setArcHeight(80)
            getScalaFill(r.getFill()) match {
              case emptyColor =>
                if (isOnEdge(i,basicColor)) contextMenu.getItems.addAll(addBasic)
              case startColor =>
                System.out.println("START")
              case basicColor =>
                if (isOnEdge(i,emptyColor)) contextMenu.getItems.addAll(deleteBasic)
                contextMenu.getItems.addAll(special, start, end)
                System.out.println("OBICNA")
              case specColor =>
                contextMenu.getItems.addAll(basic)
                System.out.println("SPECIJALNA")
              case endColor =>
                System.out.println("KRAJ")

            }
          }
            contextMenu.show(stage, a.screenX, a.screenY)
        }
      }
    })
  }

  def loadLevel(src:String, fromFile:Boolean): Unit ={
    val arena=getLinesFromFile(src)
    val (blox, end, empty, spec)=getBlocks(arena)
    initialBlox = blox
    val state = ObjectProperty(State(stage, src, arena, blox, end, empty, spec))
    stage.scene.value.content = state.value.rectangles

    def playFromFile(): Unit ={
      val selectedFile = open()
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
                  case _ => startLevel()
                    0
                }))
          if (state.value.blox != null) stage.scene.value.content = state.value.rectangles
        }
      }
    }

    def startLevel(): Unit ={
      stage.scene.value.onKeyPressed = key => {
        if (List(KeyCode.W, KeyCode.S, KeyCode.A, KeyCode.D, KeyCode.UP, KeyCode.DOWN, KeyCode.LEFT, KeyCode.RIGHT).contains(key.getCode) && state.value.blox != null) {
          state.update(
            state.value.newState(
              key.getCode match {
                case KeyCode.W => 1
                case KeyCode.S => 2
                case KeyCode.A => 3
                case KeyCode.D => 4
                case KeyCode.UP => 1
                case KeyCode.DOWN => 2
                case KeyCode.LEFT => 3
                case KeyCode.RIGHT => 4
              }))
          if (state.value.blox != null) stage.scene.value.content = state.value.rectangles
        }
      }
    }

    if (fromFile) playFromFile()
    startLevel()
  }

  def createMenu(list: List[Button]): VBox={
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
  def createMainMenu(): Unit ={
    val quit=createButton("QUIT")
    quit.onAction = (e: ActionEvent) => stage.close()

    val createLevel=createButton("CREATE LEVEL")
    createLevel.onAction = (e: ActionEvent) => chooseLevel(false)

    val start=createButton("START")
    start.onAction = (e: ActionEvent) => chooseLevel(true)

    val loadFromFile=createButton("LOAD LEVEL FROM FILE")
    loadFromFile.onAction= (e:ActionEvent) =>{
      val selectedFile = open()
      if (selectedFile != null) {
      loadLevel(selectedFile.getAbsolutePath,fromFile = false)
      }
    }

    val cnt:VBox=createMenu(List(start,loadFromFile, createLevel,quit))
    cnt.getChildren.add(0,new Text {
      text = "BLOXORZ"
      margin = Insets(50, 50, 50, 50)
      style = "-fx-font: bold 35pt sans-serif"
      fill = new LinearGradient(
        endX = 0,
        stops = Stops(Color.Red, Color.DarkGray))
      effect = new DropShadow {
        color = Color.DarkRed
        radius = 15
        spread = 0.25
        }
    })
  }

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      width = 600
      height = 600
      }
    createMainMenu()
    }

}
