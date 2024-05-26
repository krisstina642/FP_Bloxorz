package game

import game.Bloxorz.bloxSize
import game.Moves.Moves
import javafx.util.Duration
import scalafx.Includes._
import scalafx.animation.{RotateTransition, Transition, TranslateTransition}
import scalafx.geometry.Point3D
import scalafx.scene.shape.Box
import scalafx.scene.transform.Rotate

class TransformCalculations() {

  def calculate(direction: Moves, blox: List[(Double, Double)], emptyBloxList: List[(Double, Double)], bloxBox: Box): Seq[Transition] = {
    val bloxa: (Double, Double) = blox.head
    val bloxb: (Double, Double) = blox.tail.head
    val emptya: Boolean = emptyBloxList.contains(bloxa._1, bloxa._2)
    val emptyb: Boolean = emptyBloxList.contains(bloxb._1, bloxb._2)

    def createRotateransition(_angle: Int, rotateAxis: Point3D) = new RotateTransition(Duration.seconds(0.5), bloxBox) {
      toAngle = _angle; axis = rotateAxis
    }

    def createTranslateTransition(_toX: Int = 0, _toY: Int = 0)= new TranslateTransition(Duration.seconds(0.5), bloxBox) {
      toZ = bloxBox.getTranslateZ + bloxSize / 6
      toY = bloxBox.getTranslateY + _toY * (bloxSize / 2)
      toX = bloxBox.getTranslateX + _toX * (bloxSize / 2)
    }

    def createTransition(_angle: Int, rotateAxis: Point3D, _toX: Int = 0, _toY: Int = 0) = Seq (createRotateransition(_angle, rotateAxis), createTranslateTransition(_toX,_toY))
    if (emptya && emptyb) return Seq(createTranslateTransition())
    (direction, bloxa._1 != bloxb._1, bloxa._2 != bloxb._2, emptyBloxList.contains(bloxa._1, bloxa._2)
      || emptyBloxList.contains(bloxb._1, bloxb._2)) match {
      case (Moves.Left, _, true, _) => createTransition(-90,Rotate.XAxis,0,-1)
      case (Moves.Right, _, true, _) => createTransition(90,Rotate.XAxis,0,1)
      case (Moves.Down, true, _, _) => createTransition(90,Rotate.YAxis,-1)
      case (Moves.Up, true, _, _) => createTransition(-90,Rotate.YAxis,1)
      case (_, true, false, true) => if (emptyBloxList.contains(min(bloxa._1, bloxb._1).doubleValue(), bloxa._2)) { createTransition(90,Rotate.YAxis,-1)}
      else createTransition(-90,Rotate.YAxis,1)
      case (_, false, true, true) => if (emptyBloxList.contains(bloxa._1, min(bloxa._2, bloxb._2).doubleValue())) createTransition(-90,Rotate.XAxis,0,-1)
      else createTransition(90,Rotate.XAxis,0,1)
      case (_, _, _, _) => Seq(createTranslateTransition())

    }
  }
}
