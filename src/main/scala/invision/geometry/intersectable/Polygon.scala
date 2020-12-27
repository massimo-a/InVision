package invision.geometry.intersectable

import invision.geometry.Ray
import invision.util.Vec3

import scala.math.abs

/** A convex polygon defined by an array of vertices
 *  The vertices are required to be co-linear and the polygon must be convex
 *
 *  @author Massimo Angelillo
 *  @constructor create a new convex polygon with the array of points given
 *  @param vertices an array of all the vertex points that make up the convex polygon
 */
final case class Polygon(vertices: Array[Vec3]) extends Intersectable {
  private val firstVertex = vertices(0)
  private val lastVertex = vertices(vertices.length - 1)
  val normal: Vec3 = ((vertices(1) - firstVertex) ^ (lastVertex - firstVertex)).normalize()
  def getNormal(pt: Vec3): Vec3 = normal
  def intersectDistance(r: Ray): Double = {
    val check = r.direction*normal
    if(abs(check) < 1E-6) return -1
    val hitPtOnRay = ((firstVertex - r.origin)*normal)/check
    val hitPt3D = r.direction*hitPtOnRay + r.origin

    for(i <- 0 until vertices.length - 2) {
      val a = (vertices(i) - hitPt3D) ^ (vertices(i+1) - hitPt3D)
      val b = (vertices(i+1) - hitPt3D) ^ (vertices(i+2) - hitPt3D)
      if(a != b) {
        return -1
      }
    }
    hitPtOnRay
  }
}