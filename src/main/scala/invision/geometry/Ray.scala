package invision.geometry

import invision.util.Vec3

/** An infinite line with an origin and a direction
 *  
 *  @author Massimo Angelillo
 *  @constructor create a new ray with an origin and a direction
 *  @param origin the starting point of the ray
 *  @param pt2 the second point on the ray, which defines the direction
 */
case class Ray(origin: Vec3, pt2: Vec3) {
	val direction: Vec3 = (pt2-origin).normalize()
}