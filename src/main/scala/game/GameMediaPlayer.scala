package game

import game.GameMediaPlayer.{allSounds, mute}
import scalafx.Includes._
import scalafx.scene.media.{Media, MediaPlayer, MediaView}

import scala.collection.mutable.ListBuffer

object GameMediaPlayer {
  var mute: Boolean = false
  val allSounds:ListBuffer[MediaPlayer] = ListBuffer()

  object Sounds {
    val backgroundMusic:GameMediaPlayer = new GameMediaPlayer("sounds/gameplay.mp3", true)
    val click = new GameMediaPlayer("sounds/click.mp3",rate = 2)
    val win = new GameMediaPlayer("sounds/win.mp3")
    val lost = new GameMediaPlayer("sounds/lost.mp3")
    val block = new GameMediaPlayer("sounds/block.mp3")
  }
}

class GameMediaPlayer (path: String, endless: Boolean = false, rate: Double = 1) extends MediaView  {

  def getSound: MediaPlayer ={
    if (this.getMediaPlayer == null) {
      init()
    }
    this.getMediaPlayer
  }

  private def init(): Unit = {
    val sound: MediaPlayer = new MediaPlayer(new Media(this.getClass.getResource(path).toExternalForm))
    if (endless) sound.cycleCount = -1
    else {
      sound.onEndOfMedia = this.getSound.stop()
      allSounds += sound
    }
    sound.setRate(rate)
    this.setMediaPlayer(sound)
  }

  def play(): Unit = {
    if ((endless && !this.getSound.isMute) || (!endless && !mute)){
      this.getSound.play()
    }
  }

  def toggleEffects(): Unit ={
    mute = !mute
  }

  def toggleMusic(): Unit ={
    getSound.isMute match{
      case true =>
        getSound.mute = false
      case false =>
        getSound.mute = true
    }
  }

}
