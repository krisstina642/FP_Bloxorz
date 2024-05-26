package game

import scalafx.scene.paint.Material
import scalafx.scene.shape.{Box, DrawMode}
import Bloxorz.bloxSize
import game.BloxPosition.{BloxPosition, Polygon, X_axis, Y_axis, Z_axis}

object BloxPosition extends Enumeration {
  type BloxPosition = Value
  val Z_axis, Y_axis, X_axis, Polygon = Value
}

class CustomizedBox(xr: Double, yr: Double, phongMaterial: Material, bloxPosition: BloxPosition = Polygon) extends Box {
  width = bloxSize
  height = bloxSize
  depth = bloxSize
  bloxPosition match {
    case Polygon => depth = bloxSize / 3
    case X_axis => width = bloxSize * 2
    case Y_axis => height = bloxSize * 2
    case Z_axis => depth = bloxSize * 2
  }
  if (X_axis.equals(bloxPosition)) translateX = xr + bloxSize / 2
  else translateX = xr
  if (Y_axis.equals(bloxPosition)) translateY = yr + Bloxorz.bloxSize / 2
  else translateY = yr
  drawMode = DrawMode.Fill
  if (bloxPosition != Polygon) translateZ = -depth.toDouble / 2 - Bloxorz.bloxSize / 6
  material = phongMaterial
}
