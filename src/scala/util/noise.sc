package raytracing.util

import javax.imageio.ImageIO;
import java.io.File
import java.awt.image.BufferedImage

case class Noise(seed: Double = System.currentTimeMillis%10000) {
	def sin_rand(x: Double, y: Double): Double = {
		val d = Math.sin(x*541 + y*1103)*seed;
		return d - Math.floor(d);
	}
	def lerp(y0: Double, y1: Double, t: Double): Double = {
		return y0*(1-t) + y1*t;
	}
	def smooth(a: Vec3): Vec3 = {
		return Vec3(a.x*a.x*(3 - 2*a.x), a.y*a.y*(3 - 2*a.y), a.z*a.z*(3 - 2*a.z));
	}
	def value_noise(x: Double, y: Double): Double = {
		val id = Vec3(Math.floor(x),Math.floor(y));
		val lv = smooth(Vec3(x, y) - id);
		val b = lerp(sin_rand(id.x, id.y),sin_rand(id.x+1, id.y),lv.x);
		val t = lerp(sin_rand(id.x, id.y+1),sin_rand(id.x+1, id.y+1),lv.x);
		return lerp(b, t, lv.y);
	}
	def perlin_noise(x: Double, y: Double): Double = {
		val id = Vec3(Math.floor(x),Math.floor(y));
		val lv = smooth(Vec3(x, y) - id)
		val pt_vec = Vec3(x, y).normalize
		
		val rand_vec_b1 = Vec3.create(1.0, sin_rand(id.x, id.y), 0);
		val rand_vec_b2 = Vec3.create(1.0, sin_rand(id.x+1, id.y), 0);
		val b = lerp(rand_vec_b1*pt_vec,rand_vec_b2*pt_vec,lv.x);
		
		val rand_vec_t1 = Vec3.create(1.0, sin_rand(id.x, id.y+1), 0);
		val rand_vec_t2 = Vec3.create(1.0, sin_rand(id.x+1, id.y+1), 0);
		val t = lerp(rand_vec_t1*pt_vec, rand_vec_t2*pt_vec,lv.x);
		
		return lerp(b, t, lv.y);
	}
	//for testing purposes
	def save(name: String) {
		val im = new BufferedImage(1000, 1000, BufferedImage.TYPE_INT_RGB);
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = (perlin_noise(i/1000.0, j/1000.0)+
					perlin_noise(i*2/1000.0, j*2/1000.0)/2+
					perlin_noise(i*4/1000.0, j*4/1000.0)/4+
					perlin_noise(i*8/1000.0, j*8/1000.0)/8)/1.8
				val rgb = (pn*255).toInt*(256*256+256+1);
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File(name + ".png"));
	}
}