/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;

case class Ray(pt1: Vec3, pt2: Vec3) {
	val direction = (pt2-pt1).normalize();
	val origin = pt1;
	val equation = (t: Double) => {origin + direction*t}
}