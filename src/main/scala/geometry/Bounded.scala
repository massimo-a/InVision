/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.{abs,min,max};

final case class Bounds(position: Vec3, right: Vec3, up: Vec3, forward: Vec3) {
	val minimum: Vec3 = getVertices.reduceLeft((a: Vec3, b: Vec3) => Vec3(a.x min b.x, a.y min b.y, a.z min b.z))
	val maximum: Vec3 = getVertices.reduceLeft((a: Vec3, b: Vec3) => Vec3(a.x max b.x, a.y max b.y, a.z max b.z))
	
	def hitBox(r: Ray): Boolean = {
		val inter = intersections(r);
		return inter._2 >= inter._1;
	}
	
	// merges two bounding boxes together
	// two objects bounded by their respective bounding boxes
	// will also both be bounded by this larger box
	def merge(b: Bounds): Bounds = {
		val _min = Vec3(b.minimum.x min minimum.x, b.minimum.y min minimum.y, b.minimum.z min minimum.z);
		val _max = Vec3(b.maximum.x max maximum.x, b.maximum.y max maximum.y, b.maximum.z max maximum.z);
		val diff = _max - _min
		return Bounds(_min, Vec3(diff.x, 0, 0), Vec3(0, diff.y, 0), Vec3(0, 0, diff.z))
	}
	
	// returns an array of the points that make up the bounding
	// box defined by _min and _max
	def getVertices(): Array[Vec3] = {
		return Array(
			position, position + right, position + up, position + forward,
			position + right + up, position + right + forward, position + up + forward,
			position + right + up + forward
		)
	}
	
	def translate(x: Double, y: Double, z: Double): Bounds = {
		return Bounds(position+Vec3(x, y, z), right, up, forward)
	}
	
	def rotateWith(ro: Vec3 => Vec3): Bounds = {
		return Bounds(position, ro(right), ro(up), ro(forward))
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