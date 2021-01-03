package invision.geometry.intersectable

import invision.geometry.Ray
import invision.util.Vec3

/** Geometric object that can be intersected by a ray
 *  
 *  @author Massimo Angelillo
 */
trait Intersectable {
	/** 
	 *  @return the distance between the ray origin and closest point of intersection,
	 *  -1 when an intersection does not occur
	 *  
	 *  @param ray the ray being checked against for intersection
	 */
	def intersectDistance(ray: Ray): Double

	/** 
	 *  @return the normal vector to the object at a specific point
	 *  @param pt the point on the surface
	 */
	def getNormal(pt: Vec3): Vec3
}