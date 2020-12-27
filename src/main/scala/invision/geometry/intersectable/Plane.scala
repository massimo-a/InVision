package invision.geometry.intersectable

import invision.geometry.Ray
import invision.util.Vec3

import scala.math.abs

/** An infinite plane defined by a reference point and a normal vector
 *
 *  @author Massimo Angelillo
 *  @constructor create a new plane
 *  @param normal the normal vector of the plane
 *  @param point the reference point of the plane
 */
final case class Plane(normal: Vec3, point: Vec3) extends Intersectable {
  def getNormal(pt: Vec3): Vec3 = normal
  def intersectDistance(r: Ray): Double = {
    val check = r.direction*normal
    if(abs(check) < 1E-6) return -1
    ((point - r.origin)*normal)/check
  }
}