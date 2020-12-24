/*
** Author:  Massimo Angelillo
**
** A class that handles generating noise through
** the use of noise functions. Included implementations
** are value noise, fractal noise and Worley noise
*/

package raytracing.util
import annotation.tailrec;

trait Noise
final case class Value(seed: Double) extends Noise

case object Value {
	def apply(seed: String): Value = {
		Value(seed.map(_.toByte).foldLeft("")(_+_).toDouble%1E8)
	}
}
final case class Worley(arr: Array[Vec3], distance: (Vec3, Vec3) => Double = Distance.euclid) extends Noise

object Noise {
	private def sin_rand(seed: Double, x: Double, y: Double = 0.0, z: Double = 0.0): Double = {
		val d = Math.sin(x*1111*1111 + y*1111 + z)*seed;
		d - Math.round(d);
	}
	private def lerp(y0: Double, y1: Double, t: Double): Double = {
		y0*(1-t) + y1*t;
	}
	private def smooth(a: Vec3): Vec3 = {
		a.map(x => smooth(x))
	}
	private def smooth(a: Double): Double = {
		a*a*a*(a*(a*6-15)+10)
	}
	
	def get(n: Noise, x: Double, y: Double = 0, z: Double = 0): Double = {
		n match {
			case Value(s) =>
				val id = Vec3(Math.floor(x),Math.floor(y));
				val lv = smooth(Vec3(x, y) - id);
				val b = lerp(sin_rand(s, id.x, id.y),sin_rand(s, id.x+1, id.y),lv.x);
				val t = lerp(sin_rand(s, id.x, id.y+1),sin_rand(s, id.x+1, id.y+1),lv.x);
				lerp(b, t, lv.y);
			case Worley(arr, dist) =>
				arr.foldLeft(1.1){(prev, curr) => Math.min(prev, dist(curr, Vec3(x, y, z)))}
			case _ => 1.0
		}
	}
	@tailrec private def fractalize(n: Noise, octaves: Int, x: Double, y: Double, total: Double, l: Double, p: Double, maxValue: Double): Double = {
		if(octaves == 0) {
			return total/(maxValue + p)
		}
		fractalize(n, octaves-1, x, y, total + (Noise.get(n, x*l, y*l)+1)*p/2, l*2, p*0.5, maxValue + p)
	}
	@tailrec private def fractalizeWith(f: Double=>Double, n: Noise, octaves: Int, x: Double, y: Double, total: Double, l: Double, p: Double, maxValue: Double): Double = {
		if(octaves == 0) {
			return total/(maxValue + p)
		}
		fractalizeWith(f, n, octaves-1, x, y, total + f(Noise.get(n, x*l, y*l))*p, l*2, p*0.5, maxValue + p)
	}
	def fractalize(n: Noise, octaves: Int, x: Double, y: Double): Double = {
		fractalize(n, octaves, x, y, 0, 2.0, 0.5, 0)
	}
	def fractalizeWith(f: Double=>Double, n: Noise, octaves: Int, x: Double, y: Double): Double = {
		fractalizeWith(f, n, octaves, x, y, 0, 2.0, 0.5, 0)
	}
}
object Distance {
	private def smooth(a: Double): Double = {
		a*a*a*(a*(a*6-15)+10)
	}
	def euclid(v: Vec3, u: Vec3): Double = {
		(v - u).magnitude()
	}
	def manhattan(v: Vec3, u: Vec3): Double = {
		val w = (v - u).map(Math.abs)
		w.x + w.y + w.z
	}
	def pNorm(v: Vec3, u: Vec3, p: Double): Double = {
		val w = (v - u).map(a => Math.pow(Math.abs(a), p))
		Math.pow(w.x + w.y + w.z, 1.0/p)
	}
	def chebyshev(v: Vec3, u: Vec3): Double = {
		val w = (v - u).map(Math.abs)
		Math.max(Math.max(w.x, w.y), w.z)
	}
}