package invision.scene.renderable

import invision.geometry.Ray
import invision.geometry.intersectable.Intersectable
import invision.util.Vec3

case object NilLight extends Light {
  def position: Vec3 = Vec3()
  def shape: Intersectable = null
  def color: Vec3 = Vec3(1, 1, 1)
  override def intersectDistance(ray: Ray): Double = {
    -1.0
  }

  def samplePointOnLight(direction: Vec3): Vec3 = Vec3()

  def size: Double = 0
}
