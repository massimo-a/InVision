package invision.scene.renderable

import invision.geometry.intersectable.{Intersectable, Sphere}
import invision.util.Vec3

final case class BallLight(r: Double, x: Double, y: Double, z: Double, color: Vec3 = Vec3(1, 1, 1)) extends Light {
  def position: Vec3 = Vec3(x, y, z)
  def shape: Intersectable = Sphere(r, position)
  def size: Double = r
}