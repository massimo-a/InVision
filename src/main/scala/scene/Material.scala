/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.util.Vec3;
import scala.math.{random,Pi,cos,sin,sqrt,max,min};

sealed trait Material {
	def scatter(in: Vec3, n: Vec3): Vec3;
	def albedo: Double;
}

final case class Diffuse(albedo: Double = 0.9) extends Material {
	def scatter(in: Vec3, n: Vec3): Vec3 = {
		val r = random
		val theta = random*2*Pi
		val randVec = Vec3(r*cos(theta), r*sin(theta), sqrt(1-r*r))
		return (n + randVec*Math.signum(randVec*n)).normalize
	}
}
final case class Gloss(albedo: Double = 0.9, roughness: Double = 0.0) extends Material {
	def scatter(in: Vec3, n: Vec3): Vec3 = {
		val r = random
		val theta = random*2*Pi
		val randVec = Vec3(r*cos(theta), r*sin(theta), sqrt(1-r*r))
		val bounce = in.reflect(n)
		return (bounce + (randVec*roughness)*Math.signum(randVec*bounce)).normalize;
	}
}
final case class Transparency(albedo: Double = 0.9, roughness: Double = 0.0) extends Material {
	def scatter(in: Vec3, n: Vec3): Vec3 = {
		val r = random
		val theta = random*2*Pi
		val randVec = Vec3(r*cos(theta), r*sin(theta), sqrt(1-r*r))
		val bounce = -in
		return (bounce + (randVec*roughness)*Math.signum(randVec*bounce)).normalize;
	}
}