package invision.scene.renderable

import invision.geometry.Ray
import invision.geometry.intersectable.Intersectable
import invision.util.Vec3

trait Light extends Intersectable {
  def shape: Intersectable
  def position: Vec3
  def color: Vec3
  override def intersectDistance(ray: Ray): Double = {
    shape.intersectDistance(ray)
  }

  override def getNormal(pt: Vec3): Vec3 = {
    shape.getNormal(pt)
  }
}