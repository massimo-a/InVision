/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.util.Vec3;
import scala.math.{random,abs,Pi,max,log,cos,sin,sqrt};

trait Shader {
	val color: Vec3=>Vec3;
	val albedo: Double;
	def scatterLight(in: Vec3, n: Vec3): Vec3 = {
		val r = random;
		val theta = random;
		
		val x = r*cos(theta);
		val y = r*sin(theta);
		val z = sqrt(1 - r*r);
		
		val vec = Vec3(x, y, z);
		if(vec*n < 0) return -vec
		return vec
	}
	def brdf(in: Vec3, out: Vec3, n: Vec3): Double = {
		albedo;
	}
}

case class Diffuse(color: Vec3 => Vec3, albedo: Double = 1.0) extends Shader

case class Gloss(
	color: Vec3 => Vec3,
	roughness: Double = 1.0,
	albedo: Double = 1.0
) extends Shader {
	override def scatterLight(in: Vec3, n: Vec3): Vec3 = {
		val r = random;
		val theta = random;
		val refl = in.reflect(n);
		val x = r*cos(theta);
		val y = r*sin(theta);
		val vec = if(Vec3(x, y)*refl < 0) -Vec3(x, y) else Vec3(x, y);
		return (refl + vec*roughness).normalize;
	}
}

case class Transparency(
	color: Vec3 => Vec3,
	roughness: Double = 1.0,
	albedo: Double = 1.0
) extends Shader {
	override def scatterLight(in: Vec3, n: Vec3): Vec3 = {
		val r = random;
		val theta = random;
		val transmit = -in;
		val x = r*cos(theta);
		val y = r*sin(theta);
		val vec = if(Vec3(x, y)*transmit < 0) -Vec3(x, y) else Vec3(x, y);
		return (transmit + vec*roughness).normalize;
	}
}

case class Scatter(
	color: Vec3 => Vec3,
	albedo: Double = 1.0,
	reflectivity: Double,
	transparency: Double
) extends Shader {
	override def scatterLight(in: Vec3, n: Vec3): Vec3 = {
		val r = random;
		val theta = random;
		val choice = random;
		
		val x = r*cos(theta);
		val y = r*sin(theta);
		
		if(random < reflectivity) {
			val vec = if(Vec3(x, y)*(in.reflect(n)) < 0) -Vec3(x, y) else Vec3(x, y);
			return vec
		} else if(random < (1 - reflectivity - transparency)) {
			val z = sqrt(1 - r*r);
			val vec = Vec3(x, y, z);
			if(vec*n < 0) return -vec
			return vec
		} else {
			val vec = if(Vec3(x, y)*(-in) < 0) -Vec3(x, y) else Vec3(x, y);
			return vec;
		}
	}
}