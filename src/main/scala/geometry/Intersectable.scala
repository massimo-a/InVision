/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.{sqrt,abs,max,min,cos,sin};

trait Intersectable {
	/* 
	** intersectDistance returns the distance between the closest
	** intersection point of a geometric object and a ray, and the ray origin.
	** The intersection point must lie along the ray's path, and not be negative.
	** A return of -1 indicates no intersection
	*/
	def intersectDistance(r: Ray): Double;
	
	/*
	** intersectPoint returns the actual 3D point, within the world space,
	** where a ray intersects an object.
	*/
	def intersectPoint(ray: Ray): Vec3 = {
		val d = intersectDistance(ray);
		if(d == -1) return null;
		return ray.equation(d);
	}
	def getNormal(pt: Vec3): Vec3;
}

final case class Plane(normal: Vec3, point: Vec3) extends Intersectable {
	def getNormal(pt: Vec3): Vec3 = {return normal}
	def intersectDistance(r: Ray): Double = {
		val check = r.direction*normal
		if(abs(check) < 1E-6) return -1
		return ((point - r.origin)*normal)/check
	}
}

final case class Sphere(radius: Double, center: Vec3) extends Intersectable {
	def getNormal(pt: Vec3): Vec3 = {return (pt - center).normalize}
	def intersectDistance(r: Ray): Double = {
		val b = 2*(r.direction*(r.origin - center))
		val c = (r.origin - center)*(r.origin - center) - radius*radius
		val disc = b*b - 4*c
		if(disc < 0) return -1;
		val t1 = -b/2 - sqrt(disc)
		val t2 = -b/2 + sqrt(disc)
		if(t1 < 0 && t2 > 0) {
			return t2
		} else if(t1 > 0) {
			return t1
		} else return -1
	}
}

final case class Triangle(vertex1: Vec3, vertex2: Vec3, vertex3: Vec3) extends Intersectable {
	val normal = ((vertex2 - vertex1) ^ (vertex3  - vertex1)).normalize
	def getNormal(pt: Vec3): Vec3 = {return normal}
	def intersectDistance(r: Ray): Double = {
		val check = r.direction*normal
		if(abs(check) < 1E-6) return -1
		val hitPtOnRay = ((vertex1 - r.origin)*normal)/check
		val hitPt3D = (r.direction*hitPtOnRay + r.origin)
		val a = (vertex1 - hitPt3D) ^ (vertex2 - hitPt3D)
		val b = (vertex2 - hitPt3D) ^ (vertex3 - hitPt3D)
		val c = (vertex3 - hitPt3D) ^ (vertex1 - hitPt3D)
		
		val d = Math.signum(a*b)
		val e = Math.signum(b*c)
		val f = Math.signum(c*a)
		
		if(d == e && e == f) {
			return hitPtOnRay
		} else return -1
	}
}

final case class Square(vertex1: Vec3, vertex2: Vec3, vertex3: Vec3, vertex4: Vec3) extends Intersectable {
	val normal = ((vertex2 - vertex1) ^ (vertex4  - vertex1)).normalize
	def getNormal(pt: Vec3): Vec3 = {return normal}
	def intersectDistance(r: Ray): Double = {
		val check = r.direction*normal
		if(abs(check) < 1E-6) return -1
		val hitPtOnRay = ((vertex1 - r.origin)*normal)/check
		val hitPt3D = (r.direction*hitPtOnRay + r.origin)
		val a = (vertex1 - hitPt3D) ^ (vertex2 - hitPt3D)
		val b = (vertex2 - hitPt3D) ^ (vertex3 - hitPt3D)
		val c = (vertex3 - hitPt3D) ^ (vertex4 - hitPt3D)
		val d = (vertex4 - hitPt3D) ^ (vertex1 - hitPt3D)
		
		val e = Math.signum(a*b)
		val f = Math.signum(b*c)
		val g = Math.signum(c*d)
		val h = Math.signum(d*a)
		
		if(e == f && f == g && g == h) {
			return hitPtOnRay
		} else return -1
	}
}

final case class Polygon(vertices: Array[Vec3]) extends Intersectable {
	private val firstVertex = vertices(0)
	private val lastVertex = vertices(vertices.length - 1)
	val normal = ((vertices(1) - firstVertex) ^ (lastVertex  - firstVertex)).normalize
	def getNormal(pt: Vec3): Vec3 = {return normal}
	def intersectDistance(r: Ray): Double = {
		val check = r.direction*normal
		if(abs(check) < 1E-6) return -1
		val hitPtOnRay = ((firstVertex - r.origin)*normal)/check
		val hitPt3D = (r.direction*hitPtOnRay + r.origin)
		
		for(i <- 0 until vertices.length - 2) {
			val a = (vertices(i) - hitPt3D) ^ (vertices(i+1) - hitPt3D)
			val b = (vertices(i+1) - hitPt3D) ^ (vertices(i+2) - hitPt3D)
			if(a != b) {
				return -1;
			}
		}
		return hitPtOnRay
	}
}