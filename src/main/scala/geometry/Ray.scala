package raytracing.geometry;
import raytracing.util.Vec3;

/** An infinite line with an origin and a direction d
 *  
 *  @author Massimo Angelillo
 *  @constructor create a new ray with an origin and a direction
 *  @param origin the starting point of the ray
 *  @param pt2 the second point on the ray, which defines the direction
 */
case class Ray(origin: Vec3, pt2: Vec3) {
	val direction: Vec3 = (pt2-origin).normalize();
	/** 
	 *  @return the 3D point in world space after stepping a certain amount along the ray
	 *
	 */
	val equation: Double => Vec3 = (t: Double) => {origin + direction*t}
}