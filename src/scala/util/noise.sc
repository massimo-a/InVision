/*
** Author:  Massimo Angelillo
*/

package raytracing.util

import javax.imageio.ImageIO;
import java.io.File
import java.awt.image.BufferedImage
import annotation.tailrec;

case class Noise(seed: Double = System.currentTimeMillis%10000) {
	def sin_rand(x: Double, y: Double): Double = {
		val d = Math.sin(x*541 + y*1103)*seed;
		return d - Math.floor(d);
	}
	def lerp(y0: Double, y1: Double, t: Double): Double = {
		return y0*(1-t) + y1*t;
	}
	def smooth(a: Vec3): Vec3 = {
		return Vec3(a.x*a.x*a.x*(a.x*(a.x*6-15)+10),a.y*a.y*a.y*(a.y*(a.y*6-15)+10),a.z*a.z*a.z*(a.z*(a.z*6-15)+10))
	}
	def value(x: Double, y: Double): Double = {
		val id = Vec3(Math.floor(x),Math.floor(y));
		val lv = smooth(Vec3(x, y) - id);
		val b = lerp(sin_rand(id.x, id.y),sin_rand(id.x+1, id.y),lv.x);
		val t = lerp(sin_rand(id.x, id.y+1),sin_rand(id.x+1, id.y+1),lv.x);
		return (lerp(b, t, lv.y) + 1)/2.0;
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
		
		return (lerp(b, t, lv.y) + 1)/2.0;
	}
	def layered(x: Double, y: Double, octaves: Int): Double = {
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
		return arr.foldLeft(0.0){(prev, curr) => Math.max(prev, (curr - Vec3(x, y)).magnitude)}
	}
	
	//for testing purposes
	def save(name: String) {
		//(pn*255).toInt*(256*256+256+1);
		val im = new BufferedImage(1000, 1000, BufferedImage.TYPE_INT_RGB);
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = layered(i/1000.0,j/1000.0,5);
				val rgb = if(pn < 0.1) {
					13209
				} else if(pn < 0.2) {
					26316
				} else if(pn < 0.3) {
					52479
				} else if(pn < 0.4) {
					6737151
				} else if(pn < 0.5) {
					3381555
				} else if(pn < 0.6) {
					65280
				} else if(pn < 0.7) {
					6723840
				} else if(pn < 0.8) {
					6710835
				} else if(pn < 0.9) {
					3355392
				} else 16777215
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File(name + ".png"));
	}
}