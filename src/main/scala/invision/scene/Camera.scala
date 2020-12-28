package invision.scene

import invision.util.Vec3
import scala.math.tan

final case class Camera(position: Vec3)

case object Camera {
  def apply(w: Int, h: Int, fov: Double): Camera = {
    Camera(Vec3(w/2.0, h/2.0, -w/(2*tan(fov))))
  }
}