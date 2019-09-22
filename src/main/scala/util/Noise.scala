/*
** Author:  Massimo Angelillo
**
** A class that handles generating noise through
** the use of noise functions. Included implementations
** are value noise, fractal noise and Worley noise
*/

package raytracing.util

import javax.imageio.ImageIO;
import java.io.File
import java.awt.image.BufferedImage
import annotation.tailrec;

trait Noise
final case class Value(seed: Double) extends Noise
final case class Worley(arr: Array[Vec3], distance: (Vec3, Vec3) => Double = Distance.euclid) extends Noise
object Noise {
	private def sin_rand(seed: Double, x: Double, y: Double = 0.0, z: Double = 0.0): Double = {
		val d = Math.sin(x*1111*1111 + y*1111 + z)*seed;
		return (d - Math.floor(d));
	}
	private def lerp(y0: Double, y1: Double, t: Double): Double = {
		return y0*(1-t) + y1*t;
	}
	private def smooth(a: Vec3): Vec3 = {
		return a.map(x => smooth(x))
	}
	private def smooth(a: Double): Double = {
		return a*a*a*(a*(a*6-15)+10)
	}
	
	def get(n: Noise, x: Double, y: Double = 0, z: Double = 0): Double = {
		return n match {
			case Value(s) => {
				val id = Vec3(Math.floor(x),Math.floor(y));
				val lv = smooth((Vec3(x, y) - id));
				val b = lerp(sin_rand(s, id.x, id.y),sin_rand(s, id.x+1, id.y),lv.x);
				val t = lerp(sin_rand(s, id.x, id.y+1),sin_rand(s, id.x+1, id.y+1),lv.x);
				lerp(b, t, lv.y);
			}
			case Worley(arr, dist) => {
				arr.foldLeft(1.1){(prev, curr) => Math.min(prev, dist(curr, Vec3(x, y, z)))}
			}
			case _ => 1.0
		}
	}
	@tailrec private def fractalize(n: Noise, octaves: Int, x: Double, y: Double, total: Double, l: Double, p: Double, maxValue: Double): Double = {
		if(octaves == 0) {
			return total/(maxValue + p)
		}
		return fractalize(n, octaves-1, x, y, total + Noise.get(n, x*l, y*l)*p, l*2, p*0.5, maxValue + p)
	}
	def fractalize(n: Noise, octaves: Int, x: Double, y: Double): Double = {
		return fractalize(n, octaves, x, y, 0, 2.0, 0.5, 0)
	}
}
object Distance {
	private def smooth(a: Double): Double = {
		return a*a*a*(a*(a*6-15)+10)
	}
	def euclid(v: Vec3, u: Vec3): Double = {
		(v - u).magnitude
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
object TestNoise {
	def testWorley() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		
		val blocks = 10;
		val pts = new Array[Vec3](blocks*blocks);
		for(i <- 0 until blocks) {
			for(j <- 0 until blocks) {
				pts(i + j*blocks) = Vec3((i+Math.random)/blocks, (j+Math.random)/blocks)
			}
		}
		
		val n = Array(Worley(pts), Worley(pts, Distance.chebyshev), Worley(pts, Distance.manhattan))
		for(k <- 0 until n.length) {
			for(i <- 0 until 1000) {
				for(j <- 0 until 1000) {
					val pn = Noise.get(n(k), i/1000.0, j/1000.0)*1.4142*5;
					
					val col = (Vec3(1,1,1).lerp(Vec3(0,0,0), pn))*255;
					
					val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
					
					im.setRGB(i, j, rgb)
				}
			}
			ImageIO.write(im, "png", new File("worley_test_" + k + ".png"));
		}
	}
	
	def testValue() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		val n = Value(14589124)
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = Noise.get(n, i/200.0, j/200.0);
				val col = (Vec3(1,1,1).lerp(Vec3(0,0,0), pn))*255;
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File("value_test.png"));
	}
	
	def testValueFractal() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		val n = Value(14589124)
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = Noise.fractalize(n, 10, i/200.0, j/200.0);
				val col = (Vec3(1,1,1).lerp(Vec3(0,0,0), pn))*255;
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File("fractal_value_test.png"));
	}
	
	def main(args: Array[String]): Unit = {
		println("started worley test...")
		testWorley()
		println("completed worley test")
		println("started value test...")
		testValue()
		println("completed value test")
		println("started fractal value test...")
		testValueFractal()
		println("completed fractal value test")
		println("completed all tests")
	}
}