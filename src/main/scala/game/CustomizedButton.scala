package game

import game.Bloxorz.createMainMenu
import game.GameMediaPlayer.Sounds
import scalafx.scene.control.Button
import scalafx.scene.text.Font

object CustomizedButton {
  val toggleMusic: CustomizedButton = new CustomizedButton("TOGGLE SOUND", Sounds.backgroundMusic.toggleMusic)
  val toggleEffects: CustomizedButton = new CustomizedButton("TOGGLE EFFECTS", Sounds.backgroundMusic.toggleEffects)
  val mainMenuButton: CustomizedButton = new CustomizedButton("MAIN MENU", createMainMenu)
  val mainMenuButtonSmall: CustomizedButton = new CustomizedButton("BACK", createMainMenu, _prefWidth = 100, _translateX = 13, _translateY = Bloxorz.sceneHeight / 2)
}

class CustomizedButton(name: String, function: () => Unit, _prefWidth: Double = 350, _prefHeight: Double = 50, _translateX: Double = 0, _translateY: Double = 0) extends Button {
  text = name
  font = Font.apply(20)
  prefWidth = _prefWidth
  prefHeight = _prefHeight
  translateX = _translateX
  translateY = _translateY
  onAction = _ => {
    Sounds.click.play()
    function()
  }
}

