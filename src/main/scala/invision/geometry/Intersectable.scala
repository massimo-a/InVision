package invision.geometry

import invision.util.Vec3

import scala.math.{abs, sqrt}

/** Geometric object that can be intersected by a ray
 *  
 *  @author Massimo Angelillo
 */
trait Intersectable {
	/** 
	 *  @return the distance between the ray origin and closest point of intersection,
	 *  -1 when an intersection does not occur
	 *  
	 *  @param ray the ray being checked against for intersection
	 */
	def intersectDistance(ray: Ray): Double

	/** 
	 *  @return the 3D point in world space where the ray intersects the object,
	 *  null when no intersection occurs
	 *  @param ray the ray being checked against for intersection
	 */
	def intersectPoint(ray: Ray): Vec3 = {
		val d = intersectDistance(ray)
		if (d == -1) return null
		ray.origin + ray.direction*d
	}

	/** 
	 *  @return the normal vector to the object at a specific point
	 *  @param pt the point on the surface
	 */
	def getNormal(pt: Vec3): Vec3
}

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

/** A triangle defined by three colinear vertices
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

/** A quadrilateral defined by four colinear vertices
 *  The vertices are required to be colinear
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