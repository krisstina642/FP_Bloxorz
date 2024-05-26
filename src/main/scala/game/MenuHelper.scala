package game

import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.TextField
import scalafx.scene.effect.DropShadow
import scalafx.scene.paint.{Color, LinearGradient, Stops}
import scalafx.scene.text.{Font, Text}

object MenuHelper {

  val winText: Text = new Text {
    text = "YOU WIN"
    margin = Insets(35, 50, 70, 50)
    style = "-fx-font: bold 40pt sans-serif"
    fill = new LinearGradient(endX = 0, stops = Stops(Color.White, Color.DarkGray))
    effect = shadow(Color.DarkGray)
  }

  val pauseText: Text = new Text {
    text = "PAUSE"
    margin = Insets(35, 50, 70, 50)
    style = "-fx-font: bold 40pt sans-serif"
    fill = new LinearGradient(endX = 0, stops = Stops(Color.White, Color.DarkGray))
    effect = shadow(Color.DarkGray)
  }

  val highscores: Text = new Text {
    text = "HIGH SCORES"
    margin = Insets(30, 50, 30, 50)
    style = "-fx-font: bold 40pt sans-serif"
    fill = new LinearGradient(endX = 0, stops = Stops(Color.White, Color.DarkGray))
    effect = shadow(Color.DarkGray)
  }

  val failText: Text = new Text {
    text = "FAIL"
    margin = Insets(35, 50, 70, 50)
    style = "-fx-font: bold 40pt sans-serif"
    fill = new LinearGradient(endX = 0, stops = Stops(Color.Red, Color.DarkGray))
    effect = shadow(Color.DarkRed)
  }

  val winInsertName: TextField = new TextField() {
    alignment = Pos.Center
    font = Font.apply(20)
    promptText = "Enter your Name"
    maxWidth = 350
    focusTraversable = false
  }

  var currentNumberOfMoves = 0

  val numOfMoves: Text = new Text {
    text = "MOVES: 0"
    style = "-fx-font: bold 20pt sans-serif"
    fill = Color.Yellow
    translateX = 5
    translateY = 25
  }

  val pauseButton: Text = new Text {
    text = "PAUSE"
    style = "-fx-font: bold 20pt sans-serif"
    fill = Color.Yellow
    translateX = Bloxorz.sceneWidth - 100
    translateY = 25
  }

  val bloxorzStartText: Text = new Text {
    text = "BLOXORZ"
    margin = Insets(0, 50, 50, 50)
    style = "-fx-font: bold 60pt sans-serif"
    fill = new LinearGradient(
      endX = 0,
      stops = Stops(Color.Red, Color.DarkGray))
      effect = new DropShadow {
      color = Color.DarkRed
      radius = 15
      spread = 0.25
    }
  }

  def shadow(_color: Color) = new DropShadow {
    color = _color
    radius = 15
    spread = 0.25
  }

  def resetNumOfMoves(): Text = {
    currentNumberOfMoves = 0
    numOfMoves.text = "MOVES: 0"
    numOfMoves
  }

  def incNumOfMoves(): Text = {
    currentNumberOfMoves = currentNumberOfMoves + 1
    numOfMoves.text = "MOVES: " + currentNumberOfMoves
    numOfMoves
  }
}
