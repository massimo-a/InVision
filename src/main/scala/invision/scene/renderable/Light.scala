package invision.scene.renderable

import invision.geometry.Ray
import invision.geometry.intersectable.Intersectable
import invision.util.Vec3

trait Light {
  def shape: Intersectable
  def position: Vec3
  def color: Vec3
  def intersectDistance(ray: Ray): Double = {
    shape.intersectDistance(ray)
  }
}