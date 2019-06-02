package raytracing.util

case class Noise(seed: Double = System.currentTimeMillis%10000) {
	def sin_rand(x: Double, y: Double): Double = {
		val d = Math.sin(x*541 + y*1103)*seed;
		return d - Math.floor(d);
	}
	def lerp(y0: Double, y1: Double, t: Double): Double = {
		return y0*(1-t) + y1*t;
	}
	def smooth(a: Double): Double = {
		return a*a*(3 - 2*a)
	}
	def value_noise(x: Double, y: Double): Double = {
		val id = Vec3(Math.floor(x),Math.floor(y));
		val lv = Vec3(x, y) - id
		val b = lerp(sin_rand(id.x, id.y),sin_rand(id.x+1, id.y),lv.x);
		val t = lerp(sin_rand(id.x, id.y+1),sin_rand(id.x+1, id.y+1),lv.x);
		return lerp(b, t, lv.y);
	}
}