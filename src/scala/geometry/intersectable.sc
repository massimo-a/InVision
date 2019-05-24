/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.{abs,min,max};

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
		return ray.origin + ray.direction*d;
	}
	def getNormal(pt: Vec3): Vec3;
	def getAngleWithNormal(pt: Vec3, d: Vec3): Double;
}

trait Bounded {
	val minimum: Vec3;
	val maximum: Vec3;
	
	def hitBox(r: Ray): Boolean = {
		val inter = intersections(r);
		return inter._2 >= inter._1;
	}
	
	// merges two bounding boxes together
	// two objects bounded by their respective bounding boxes
	// will also both be bounded by this larger box
	def merge(min1: Vec3, max1: Vec3, min2: Vec3, max2: Vec3): (Vec3, Vec3) = {
		val _min = Vec3(min(min1.x, min2.x), min(min1.y, min2.y), min(min1.z, min2.z));
		val _max = Vec3(max(max1.x, max2.x), max(max1.y, max2.y), max(max1.z, max2.z));
		return (_min, _max)
	}
	
	// returns an array of the points that make up the bounding
	// box defined by _min and _max
	def getOOBB(_min: Vec3, _max: Vec3): Array[Vec3] = {
		return Array(
			_min,Vec3(_max.x,_min.y,_min.z),Vec3(_max.x,_min.y,_max.z),Vec3(_min.x,_min.y,_max.z),
			Vec3(_min.x,_max.y,_min.z),Vec3(_max.x,_max.y,_min.z),_max,Vec3(_min.x,_max.y,_max.z)
		)
	}
	def intersections(r: Ray): (Double, Double) = {
		var tmin = Double.NegativeInfinity;
		var tmax = Double.PositiveInfinity;
		if(r.direction.x != 0.0) {
			val tx1 = (minimum.x - r.origin.x)/r.direction.x;
			val tx2 = (maximum.x - r.origin.x)/r.direction.x;
			tmin = max(tmin, min(tx1, tx2));
			tmax = min(tmax, max(tx1, tx2));
		}
		if(r.direction.y != 0.0) {
			val ty1 = (minimum.y - r.origin.y)/r.direction.y;
			val ty2 = (maximum.y - r.origin.y)/r.direction.y;
			tmin = max(tmin, min(ty1, ty2));
			tmax = min(tmax, max(ty1, ty2));
		}
		if(r.direction.z != 0.0) {
			val tz1 = (minimum.z - r.origin.z)/r.direction.z;
			val tz2 = (maximum.z - r.origin.z)/r.direction.z;
			tmin = max(tmin, min(tz1, tz2));
			tmax = min(tmax, max(tz1, tz2));
		}
		return (tmin, tmax);
	}
}
trait Planar extends Intersectable {
	val normal: Vec3;
	val referencePoint: Vec3;
	def contains(pt: Vec3): Boolean;
	def intersectDistance(ray: Ray): Double = {
		val n = -normal;
		val a = (referencePoint - ray.origin) * n;
		val d = ray.direction * n;
		if(d > 0 && a >= 0) {
			if(contains(ray.origin + (ray.direction*(a/d)))) return a/d;
		}
		return -1;
	}
	def getNormal(pt: Vec3): Vec3 = {
		normal;
	}
	def getAngleWithNormal(pt: Vec3, d: Vec3): Double = {
		abs(d*normal);
	}
}
case class Ray(pt1: Vec3, pt2: Vec3) {
	val direction = (pt2-pt1).normalize();
	val origin = pt1;
	val equation = (t: Double) => {origin + direction*t}
}