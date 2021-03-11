package invision.geometry.intersectable

import invision.geometry.Ray
import invision.util.Vec3

import scala.math.abs

/** A triangle defined by three co-linear vertices
 *
 *  @author Massimo Angelillo
 *  @constructor create a new triangle with the three points given
 *  @param vertex1 the first vertex of the triangle
 *  @param vertex2 the second vertex of the triangle
 *  @param vertex3 the third vertex of the triangle
 */
final case class Triangle(vertex1: Vec3, vertex2: Vec3, vertex3: Vec3) extends Intersectable {
  val normal: Vec3 = ((vertex2 - vertex1) ^ (vertex3 - vertex1)).normalize()

  def getNormal(pt: Vec3): Vec3 = normal

  def pointInTriangle(pt: Vec3): Boolean = {
    val a = (vertex1 - pt) ^ (vertex2 - pt)
    val b = (vertex2 - pt) ^ (vertex3 - pt)
    val c = (vertex3 - pt) ^ (vertex1 - pt)

    val d = Math.signum(a*b)
    val e = Math.signum(b*c)
    val f = Math.signum(c*a)

    if(d == e && e == f) {
      true
    } else false
  }

  def intersectDistance(r: Ray): Double = {
    val check = r.direction*normal
    if(abs(check) < 1E-6) return -1
    val hitPtOnRay = ((vertex1 - r.origin)*normal)/check
    val hitPt3D = r.direction*hitPtOnRay + r.origin
    val a = (vertex1 - hitPt3D) ^ (vertex2 - hitPt3D)
    val b = (vertex2 - hitPt3D) ^ (vertex3 - hitPt3D)
    val c = (vertex3 - hitPt3D) ^ (vertex1 - hitPt3D)

    val d = Math.signum(a*b)
    val e = Math.signum(b*c)
    val f = Math.signum(c*a)

    if(d == e && e == f) {
      hitPtOnRay
    } else -1
  }
}