package invision.scene.material

import invision.util.Vec3

/** 
 *  @author Massimo Angelillo
 */
trait Material {
	def scatter(in: Vec3, n: Vec3): Vec3
	def brdf(in: Vec3, out: Vec3, n: Vec3): Double
}