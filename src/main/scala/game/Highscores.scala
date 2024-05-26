package game

import scalafx.geometry.Insets
import scalafx.scene.paint.{Color, LinearGradient, Stops}
import scalafx.scene.text.Text

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.io.Source

case class Highscores(){
  val HIGHSCORES = "HighScores.txt"
  val HIGHSCORESNAME = "HighScoresName.txt"
  if (!Files.exists(Paths.get(HIGHSCORESNAME)) || !Files.exists(Paths.get(HIGHSCORES))) {
    hardcodedData()
  }
  private val bufferedSource = Source.fromFile(HIGHSCORESNAME)
  val NAMES:List[String] = bufferedSource.getLines.toList
  private val bufferedSource2 = Source.fromFile(HIGHSCORES)
  val SCORES:List[String] = bufferedSource2.getLines.toList
  bufferedSource.close
  bufferedSource2.close

  final def addScore(name: String, score: Int = MenuHelper.currentNumberOfMoves): Unit ={
    @tailrec
    def addScoreRec(iter:Int=0,list: List[String] = SCORES): (List[String],List[String]) = {
      if (list.isEmpty) (NAMES,SCORES)
      else if (list.head.toInt >= score) (
        (NAMES.take(iter) ++ List(name) ++ NAMES.drop(iter)).take(NAMES.size),
        (SCORES.take(iter) ++ List(score.toString) ++ SCORES.drop(iter)).take(SCORES.size)
        )
      else addScoreRec(iter+1,list.tail)
    }
    writeToFile(addScoreRec()._1,addScoreRec()._2)
  }

  final def writeToFile(names:List[String], scores:List[String]): Unit ={
    val writerName = new PrintWriter(new File(HIGHSCORESNAME))
    val writerScore = new PrintWriter(new File(HIGHSCORES))
    @tailrec
    def writeToF(listName :List[String]=names, listScore :List[String]=scores): Unit ={
      if (listName.isEmpty) ()
      else {
        writerName.write(listName.head+ "\n")
        writerScore.write(listScore.head + "\n")
        writeToF(listName.tail, listScore.tail)
      }
    }
    writeToF()
    writerName.close()
    writerScore.close()
  }

  def hardcodedData(): Unit ={
    val writer = new PrintWriter(new File("HighScoresName.txt"))
    writer.write("Player1\nPlayer2\nPlayer3\nPlayer4\nPlayer5\nPlayer6\nPlayer7\nPlayer8")
    writer.close()
    val writer2 = new PrintWriter(new File("HighScores.txt"))
    writer2.write("160\n170\n180\n190\n200\n220\n240\n260")
    writer2.close()
  }

  def getResults(): List[Text] = {

    @tailrec
    def res(listName :List[String]=NAMES, listScore :List[String]=SCORES, acc: List[Text] = Nil): List[Text] ={
      if (listName.isEmpty) acc
      else {
       res(listName.tail, listScore.tail, acc ++ List(new Text (listName.head + " " + listScore.head){
         margin = Insets(7, 50, 7, 50)
         style = "-fx-font: bold 20pt sans-serif"
         fill = new LinearGradient(endX = 0, stops = Stops(Color.Yellow, Color.OrangeRed))
       }))
      }
    }
    res()
  }
}

