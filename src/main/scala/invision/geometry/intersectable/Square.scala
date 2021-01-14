package invision.geometry.intersectable

import invision.geometry.Ray
import invision.util.Vec3

import scala.math.abs

/** A quadrilateral defined by four collinear vertices
 *  The vertices are required to be collinear
 *
 *  @author Massimo Angelillo
 *  @constructor create a new quadrilateral with the four points given
 *  @param vertex1 the first vertex of the quadrilateral
 *  @param vertex2 the second vertex of the quadrilateral
 *  @param vertex3 the third vertex of the quadrilateral
 *  @param vertex4 the fourth vertex of the quadrilateral
 */
final case class Square(vertex1: Vec3, vertex2: Vec3, vertex3: Vec3, vertex4: Vec3) extends Intersectable {
  val normal: Vec3 = ((vertex2 - vertex1) ^ (vertex4 - vertex1)).normalize()
  def getNormal(pt: Vec3): Vec3 = normal
  def intersectDistance(r: Ray): Double = {
    val check = r.direction*normal
    if(abs(check) < 1E-6) return -1
    val hitPtOnRay = ((vertex1 - r.origin)*normal)/check
    val hitPt3D = r.direction*hitPtOnRay + r.origin
    val a = (vertex1 - hitPt3D) ^ (vertex2 - hitPt3D)
    val b = (vertex2 - hitPt3D) ^ (vertex3 - hitPt3D)
    val c = (vertex3 - hitPt3D) ^ (vertex4 - hitPt3D)
    val d = (vertex4 - hitPt3D) ^ (vertex1 - hitPt3D)

    val e = Math.signum(a*b)
    val f = Math.signum(b*c)
    val g = Math.signum(c*d)
    val h = Math.signum(d*a)

    if(e == f && f == g && g == h) {
      hitPtOnRay
    } else -1
  }
}
