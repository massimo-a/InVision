/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.{sqrt,abs};

case class Sphere(
	x: Double=0,
	y: Double=0,
	z: Double=0,
	radius: Double=100
) extends Intersectable {
	val center = Vec3(x, y, z);
	def intersectDistance(ray: Ray): Double = {
		val to_sphere = ray.origin - center; 
		val b = ray.direction*to_sphere;
		val check = b*b - to_sphere.magnitude()*to_sphere.magnitude() + radius*radius;
		if(check < 0) {
			return -1;
		} else {
			val t1 = -b - sqrt(check);
			val t2 = -b + sqrt(check);
			val dist = if(t1 < 0) {
				if(t2 < 0) {
					-1
				} else t2
			} else t1
			return dist
		}
	}
	def getNormal(pt: Vec3): Vec3 = {
		(pt - center).normalize();
	}
	def getAngleWithNormal(pt: Vec3, d: Vec3): Double = {
		getNormal(pt)*d;
	}
}
case class Box(
	minx: Double, miny: Double, minz: Double,
	maxx: Double, maxy: Double, maxz: Double
) extends Intersectable with Bounded {
	val minimum = Vec3(minx, miny, minz);
	val maximum = Vec3(maxx, maxy, maxz);
	private def approx(a: Double, b: Double, err: Double): Boolean = {
		return (abs(a - b) < err)
	}
	def intersectDistance(r: Ray): Double = {
		val t = intersections(r);
		if(t._1 >= t._2) return -1
		if(t._1 < 0) {
			if(t._2 < 0) {
				return -1
			} else return t._2
		} else return t._1
	}
	def getNormal(pt: Vec3): Vec3 = {
		if(approx(pt.x, minx, 0.01)) return Vec3(-1, 0, 0);
		if(approx(pt.x, maxx, 0.01)) return Vec3(1, 0, 0);
		if(approx(pt.y, miny, 0.01)) return Vec3(0, -1, 0);
		if(approx(pt.y, maxy, 0.01)) return Vec3(0, 1, 0);
		if(approx(pt.z, minz, 0.01)) return Vec3(0, 0, -1);
		return Vec3(0, 0, 1);
	}
	def getAngleWithNormal(pt: Vec3, d: Vec3): Double = {
		return abs(getNormal(pt)*d);
	}
}