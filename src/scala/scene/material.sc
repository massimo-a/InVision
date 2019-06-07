package raytracing.scene;
import raytracing.util.Vec3;
import scala.math.{random,abs,Pi,max,log,cos,sin,sqrt};

trait Shader {
	val color: Vec3;
	val albedo: Double;
	val emission: Vec3 = Vec3(0,0,0);
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

case class Diffuse(
	red: Double = 1.0,
	green: Double = 1.0,
	blue: Double = 1.0,
	albedo: Double = 1.0
) extends Shader {
	val color = Vec3(red, green, blue);
}

case class Gloss(
	red: Double = 1.0,
	green: Double = 1.0,
	blue: Double = 1.0,
	roughness: Double = 1.0,
	albedo: Double = 1.0
) extends Shader {
	val color = Vec3(red, green, blue);
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
	red: Double = 1.0,
	green: Double = 1.0,
	blue: Double = 1.0,
	roughness: Double = 1.0,
	albedo: Double = 1.0
) extends Shader {
	val color = Vec3(red, green, blue);
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
	red: Double = 1.0,
	green: Double = 1.0,
	blue: Double = 1.0,
	albedo: Double = 1.0,
	scatter: (Vec3, Vec3) => Vec3
) extends Shader {
	val color = Vec3(red, green, blue);
	override def scatterLight(in: Vec3, n: Vec3): Vec3 = {return scatter(in, n)};
}