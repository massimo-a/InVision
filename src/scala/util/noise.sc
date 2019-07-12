/*
** Author:  Massimo Angelillo
**
** A class that handles generating noise through
** the use of noise functions. Included implementations
** are Perlin noise, value noise, fractal noise and Worley noise
*/

package raytracing.util

import javax.imageio.ImageIO;
import java.io.File
import java.awt.image.BufferedImage
import annotation.tailrec;

case class Noise(seed: Double = System.currentTimeMillis%10000) {
	def sin_rand(x: Double, y: Double = 0.0, z: Double = 0.0): Double = {
		val d = Math.sin(x*541 + y*1103 + z*1303)*seed;
		return d - Math.floor(d);
	}
	def lerp(y0: Double, y1: Double, t: Double): Double = {
		return y0*(1-t) + y1*t;
	}
	def smooth(a: Vec3): Vec3 = {
		return Vec3(a.x*a.x*a.x*(a.x*(a.x*6-15)+10),a.y*a.y*a.y*(a.y*(a.y*6-15)+10),a.z*a.z*a.z*(a.z*(a.z*6-15)+10))
	}
	def smooth(b: Double): Double = {
		return 3*b*b - 2*b*b*b
	}
	def value(x: Double, y: Double): Double = {
		val id = Vec3(Math.floor(x),Math.floor(y));
		val lv = smooth(Vec3(x, y) - id);
		val b = lerp(sin_rand(id.x, id.y),sin_rand(id.x+1, id.y),lv.x);
		val t = lerp(sin_rand(id.x, id.y+1),sin_rand(id.x+1, id.y+1),lv.x);
		return lerp(b, t, lv.y);
	}
	def perlin(x: Double, y: Double): Double = {
		val id = Vec3(Math.floor(x),Math.floor(y));
		val lv = smooth(Vec3(x, y) - id)
		val pt_vec = Vec3(x, y).normalize
		
		val rand_vec_b1 = Vec3.create(1.0, sin_rand(id.x, id.y)*2*Math.PI, 0);
		val rand_vec_b2 = Vec3.create(1.0, sin_rand(id.x+1, id.y)*2*Math.PI, 0);
		val b = lerp(rand_vec_b1*pt_vec,rand_vec_b2*pt_vec,lv.x);
		
		val rand_vec_t1 = Vec3.create(1.0, sin_rand(id.x, id.y+1)*2*Math.PI, 0);
		val rand_vec_t2 = Vec3.create(1.0, sin_rand(id.x+1, id.y+1)*2*Math.PI, 0);
		val t = lerp(rand_vec_t1*pt_vec, rand_vec_t2*pt_vec,lv.x);
		
		return lerp(b, t, lv.y);
	}
	def perlin3d(x: Double, y: Double, z: Double): Double = {
		val id = Vec3(Math.floor(x),Math.floor(y),Math.floor(z));
		val lv = smooth(Vec3(x, y, z) - id)
		val pt_vec = Vec3(x, y, z).normalize
		
		val rand_vec_b1 = Vec3.create(1.0, sin_rand(id.x, id.y, id.z)*2*Math.PI, 0);
		val rand_vec_b2 = Vec3.create(1.0, sin_rand(id.x+1, id.y, id.z)*2*Math.PI, 0);
		val rand_vec_b3 = Vec3.create(1.0, sin_rand(id.x, id.y+1, id.z)*2*Math.PI, 0);
		val rand_vec_b4 = Vec3.create(1.0, sin_rand(id.x+1, id.y+1, id.z)*2*Math.PI, 0);
		
		val b1 = lerp(rand_vec_b1*pt_vec,rand_vec_b2*pt_vec,lv.x);
		val b2 = lerp(rand_vec_b3*pt_vec,rand_vec_b4*pt_vec,lv.x);
		val b = lerp(b1, b2, lv.y);
		
		val rand_vec_t1 = Vec3.create(1.0, sin_rand(id.x, id.y, id.z+1)*2*Math.PI, 0);
		val rand_vec_t2 = Vec3.create(1.0, sin_rand(id.x+1, id.y, id.z+1)*2*Math.PI, 0);
		val rand_vec_t3 = Vec3.create(1.0, sin_rand(id.x, id.y+1, id.z+1)*2*Math.PI, 0);
		val rand_vec_t4 = Vec3.create(1.0, sin_rand(id.x+1, id.y+1, id.z+1)*2*Math.PI, 0);
		
		val t1 = lerp(rand_vec_t1*pt_vec, rand_vec_t2*pt_vec,lv.x);
		val t2 = lerp(rand_vec_t3*pt_vec, rand_vec_t4*pt_vec,lv.x);
		val t = lerp(t1, t2, lv.y);
		
		return lerp(b, t, lv.z);
	}
	def layered3d(x: Double, y: Double, z: Double, octaves: Int): Double = {
		var total = 0.0
		var l = 2.0
		var p = 0.5
		var maxValue = 0.0
		for(i <- 0 until octaves) {
			total = total + perlin3d(x*l, y*l, z*l)*p
			maxValue = maxValue + p
			l = l*2
			p = p*0.5
		}
		return total/maxValue
	}
	def fractalPerlin(x: Double, y: Double, octaves: Int): Double = {
		var total = 0.0
		var l = 2.0
		var p = 0.5
		var maxValue = 0.0
		for(i <- 0 until octaves) {
			total = total + perlin(x*l, y*l)*p
			maxValue = maxValue + p
			l = l*2
			p = p*0.5
		}
		return total/maxValue
	}
	
	def worley(arr: Array[Vec3], x: Double, y: Double): Double = {
		return arr.foldLeft(1.1){(prev, curr) => Math.min(prev, (curr - Vec3(x, y)).magnitude)}
	}
	
	//for testing purposes
	
	def testWorley(name: String, ext: String = "png") {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		val blocks = 5;
		val ptsPerBlock = 20;
		val pts = new Array[Vec3](blocks*blocks*ptsPerBlock);
		for(i <- 0 until blocks) {
			for(j <- 0 until blocks) {
				for(k <- 0 until ptsPerBlock) {
					pts(i + j*blocks + k*blocks*blocks) = Vec3((Math.random + i)/blocks, (Math.random + j)/blocks)
				}
			}
		}
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = worley(pts, i/1000.0, j/1000.0)*1.4142*5;
				val col = (Vec3(1,0,0).lerp(Vec3(0,1,0), pn))*255;
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, ext, new File(name + "." + ext));
	}
	
	def testFractal(name: String = "test", ext: String = "png") {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = (fractalPerlin(i/300.0, j/300.0, 10)+1)/2.0;
				val col = if(pn < 0.5) {
					(Vec3(72,61,139).lerp(Vec3(135,206,250), pn*2));
				} else if(pn < 0.9) {
					(Vec3(44,176,55).lerp(Vec3(90,77,65), (pn - 0.5)/0.4));
				} else Vec3(225,225,225)
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, ext, new File(name + "." + ext));
	}
}