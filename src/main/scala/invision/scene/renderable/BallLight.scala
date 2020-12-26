package invision.scene.renderable

import invision.geometry.{Intersectable, Sphere}
import invision.util.Vec3

final case class BallLight(r: Double, x: Double, y: Double, z: Double) extends Light {
  def position: Vec3 = Vec3(x, y, z)
  def shape: Intersectable = Sphere(r, position)
  def color: Vec3 = Vec3(1, 1, 1)
}