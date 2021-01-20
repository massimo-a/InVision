package invision.scene.material

import invision.util.Vec3

import scala.math.{Pi, random}

/** 
 *  @author Massimo Angelillo
 */
trait Material {
	def scatter(in: Vec3, n: Vec3): Vec3
	def albedo: Double

	/**
	 * Uniformly samples a vector in 3d.
	 * @return A vector.
	 */
	def sampleVectorOnSphere(): Vec3 = {
		val u = random
		val v = random
		val theta = Pi * 2 * u
		val phi = math.acos(2*v - 1)
		Vec3.create(1, theta, phi)
	}
}