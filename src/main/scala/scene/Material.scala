/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.util.Vec3;
import scala.math.{random,abs,Pi,max,log,cos,sin,sqrt};

trait Material
case class Diffuse() extends Material
case class Gloss(roughness: Double) extends Material
case class Transparency(roughness: Double) extends Material

object Material {
	def scatter(m: Material, in: Vec3, n: Vec3): Vec3 = {
		val r = random
		val theta = random
		val x = r*cos(theta)
		val y = r*sin(theta)
		val z = sqrt(1-r*r)
		
		val (bounce: Vec3, roughness: Double) = m match {
			case Diffuse() => (n, 1.0)
			case Gloss(rough) => (in.reflect(n), rough)
			case Transparency(rough) => (-in, rough)
		}
		
		if(Vec3(x, y, z)*bounce < 0) {
			return (bounce - Vec3(x, y, z)*roughness).normalize;
		} else {
			return (bounce + Vec3(x, y, z)*roughness).normalize;
		}
	}
	
	def brdf(m: Material, pt: Vec3, n: Vec3): Double = m match {
		case Diffuse() => 0.9
		case Gloss(rough) => 0.9
		case Transparency(rough) => 0.9
	}
}