/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.util.Vec3;
import scala.math.{random,abs,Pi,max,log,cos,sin,sqrt};

sealed trait Material
final case class Diffuse(albedo: Double = 0.9) extends Material
final case class Gloss(albedo: Double = 0.9, roughness: Double = 0.0) extends Material
final case class Transparency(albedo: Double = 0.9, roughness: Double = 0.0) extends Material

object Material {
	def scatter(m: Material, in: Vec3, n: Vec3): Vec3 = {
		val r = random
		val theta = random
		val randVec = Vec3(r*cos(theta), r*sin(theta), sqrt(1-r*r))
		
		val (bounce: Vec3, roughness: Double) = m match {
			case Diffuse(a) => (n, 1.0)
			case Gloss(a, rough) => (in.reflect(n), rough)
			case Transparency(a, rough) => (-in, rough)
		}
		return (bounce + (randVec*roughness)*Math.signum(randVec*bounce)).normalize;
	}
	
	def brdf(m: Material, pt: Vec3, n: Vec3): Double = m match {
		case Diffuse(a) => a
		case Gloss(a, rough) => a
		case Transparency(a, rough) => a
	}
}