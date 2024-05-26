package game

import scalafx.scene.{Camera, ParallelCamera, PerspectiveCamera}

trait CameraTrait extends Camera {
  val cameraXform = new Xform()
  val cameraXform2 = new Xform()
  val cameraXform3 = new Xform()
  cameraXform.children += cameraXform2
  cameraXform2.children += cameraXform3
  cameraXform3.children += this
  nearClip = 0.1
  farClip = 10000.0
}

class ParCamera( _translateX:Double=0, _translateY:Double=0) extends ParallelCamera with CameraTrait {
  translateX = _translateX
  translateY = _translateY
}

object PerCamTrait {
  val cameraDistance: Double = 1300
}

class PerCamera(fixedEye:Boolean) extends PerspectiveCamera(fixedEye) with CameraTrait {
  translateZ = -PerCamTrait.cameraDistance
  cameraXform.rz.angle = 50
  cameraXform.rx.angle = 55
  def getCam: Xform = cameraXform
}
