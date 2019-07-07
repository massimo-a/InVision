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
	
	private def intersectionOfPlane(r: Ray, norm: Vec3, pt: Vec3): Double = {
		val a = ray.direction*norm;
		if(a < 1e-6) return -1;
		return ((pt - ray.origin)*norm)/a
	}
	
	private def sign(a: Double): Int = {
		if(a < 0) return -1
		return 1
	}
	
	private def ptInsideSquare(pt: Vec3, corner: Vec3, len1: Vec3, lec2: Vec3): Boolean = {
		val sign1 = sign((corner - pt)*(corner + len1 - pt));
		val sign2 = sign((corner + len1 - pt)*(corner + len1 + len2 - pt));
		if(sign1 != sign2) return false;
		val sign3 = sign((corner + len1 + len2 - pt)*(corner + len2 - pt));
		if(sign2 != sign3) return false;
		val sign4 = sign((corner + len2 - pt)*(corner - pt));
		if(sign3 != sign4) return false;
		return true;
	}
	
	private def intersectsBox(r: Ray, corner: Vec3, len1: Vec3, len2: Vec3, len3: Vec3): Double = {
		val plane1 = (corner, len1, len2)
		val plane2 = (corner, len2, len3)
		val plane3 = (corner, len3, len1)
		val plane4 = (corner+len1+len2+len3, -len1, -len2)
		val plane5 = (corner+len1+len2+len3, -len2, -len3)
		val plane6 = (corner+len1+len2+len3, -len3, -len1)
		
		val inter1 = intersectionOfPlane(r, plane1._1, plane1._2, plane1._3);
		val pt1 = if(inter1 > 0 && ptInsideSquare(r.equation(inter1), plane1._1, plane1._2, plane1._3)) inter1
		
		val inter2 = intersectionOfPlane(r, plane2._1, plane2._2, plane2._3);
		val pt2 = if(inter2 > 0 && ptInsideSquare(r.equation(inter2), plane2._1, plane2._2, plane2._3)) inter2
		
		val inter3 = intersectionOfPlane(r, plane3._1, plane3._2, plane3._3);
		val pt3 = if(inter3 > 0 && ptInsideSquare(r.equation(inter3), plane3._1, plane3._2, plane3._3)) inter3
		
		val inter4 = intersectionOfPlane(r, plane4._1, plane4._2, plane4._3);
		val pt4 = if(inter4 > 0 && ptInsideSquare(r.equation(inter4), plane4._1, plane4._2, plane4._3)) inter4
		
		val inter5 = intersectionOfPlane(r, plane5._1, plane5._2, plane5._3);
		val pt5 = if(inter5 > 0 && ptInsideSquare(r.equation(inter5), plane5._1, plane5._2, plane5._3)) inter5
		
		val inter6 = intersectionOfPlane(r, plane6._1, plane6._2, plane6._3);
		val pt6 = if(inter6 > 0 && ptInsideSquare(r.equation(inter6), plane6._1, plane6._2, plane6._3)) inter6
		
		val end = Array(pt1, pt2, pt3, pt4, pt5, pt6).filter(x => x < 0).foldLeft(Double.PositiveInfinity){(prev, curr) => min(prev, curr)}
		if(end == Double.PositiveInfinity) return -1
		return end
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