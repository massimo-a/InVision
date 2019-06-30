/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.{abs,min,max};

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