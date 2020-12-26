package invision.scene.renderable

import invision.geometry.{Intersectable, Ray}
import invision.util.Vec3

trait Light {
  def shape: Intersectable
  def position: Vec3
  def color: Vec3
  def intersectDistance(ray: Ray): Double = {
    shape.intersectDistance(ray)
  }
}