/*
** Author:  Massimo Angelillo
** These classes deal with all 2 dimensional objects
*/

package raytracing.geometry;
import raytracing.util.Vec3;

case class Plane(referencePoint: Vec3 = Vec3(), normal: Vec3 = Vec3()) extends Planar {
	def contains(pt: Vec3): Boolean = {return true}
}
case class Triangle(pt1: Vec3, pt2: Vec3, pt3: Vec3) extends Planar {
	val referencePoint = pt1;
	val normal = ((pt2-pt1)^(pt3-pt1)).normalize;
	def contains(pt: Vec3): Boolean = {
		val v1 = pt1-pt;
		val v2 = pt2-pt;
		val v3 = pt3-pt;
		
		val w1 = v1^v2;
		val w2 = v2^v3;
		val w3 = v3^v1;
		
		if(w1*w2 < 0) return false;
		if(w2*w3 < 0) return false;
		if(w3*w1 < 0) return false;
		return true;
	}
}
case class Square(pt: Vec3, leg1: Vec3, leg2: Vec3) extends Planar {
	val referencePoint = pt;
	val normal = (leg1^leg2).normalize;
	def contains(p: Vec3): Boolean = {
		val v1 = pt-p;
		val v2 = pt+leg1-p;
		val v3 = pt+leg1+leg2-p;
		val v4 = pt+leg2-p;
		
		val w1 = v1^v2;
		val w2 = v2^v3;
		val w3 = v3^v4;
		val w4 = v4^v1;
		
		if((w1*w2) < 0) return false;
		if((w2*w3) < 0) return false;
		if((w3*w4) < 0) return false;
		if((w4*w1) < 0) return false;
		return true;
	}
}