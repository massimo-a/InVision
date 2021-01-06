package invision.scene.renderable

import invision.geometry.Ray
import invision.geometry.intersectable.Intersectable
import invision.util.Vec3

/**
 * Represents a light source in a scene.
 */
trait Light extends Intersectable {
  def shape: Intersectable
  def position: Vec3
  def color: Vec3
  def size: Double

  def intersectDistance(ray: Ray): Double = {
    shape.intersectDistance(ray)
  }

  def getNormal(pt: Vec3): Vec3 = {
    shape.getNormal(pt)
  }
}