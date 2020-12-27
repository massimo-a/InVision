package invision.geometry.intersectable

import invision.geometry.Ray
import invision.util.Vec3

import scala.math.sqrt

/** A sphere defined by a center point in 3D space and a radius
 *
 *  @author Massimo Angelillo
 *  @constructor create a new sphere with a center and a radius
 *  @param radius the radius
 *  @param center the center of the sphere in 3D world space
 */
final case class Sphere(radius: Double, center: Vec3) extends Intersectable {
  def getNormal(pt: Vec3): Vec3 = {(pt - center).normalize()}
  def intersectDistance(r: Ray): Double = {
    val b = 2*(r.direction*(r.origin - center))
    val c = (r.origin - center)*(r.origin - center) - radius*radius
    val disc = b*b - 4*c
    if(disc < 0) return -1
    val t1 = -b/2 - sqrt(disc)
    val t2 = -b/2 + sqrt(disc)
    if(t1 < 0 && t2 > 0) {
      t2
    } else if(t1 > 0) {
      t1
    } else -1
  }
}
