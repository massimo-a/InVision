package invision.scene
import invision.geometry.Ray
import invision.util.Vec3

import scala.math.random

trait Renderer {
	val world: World
	val camera: Camera
	val spp: Int
	val width: Int
	val height: Int

	def getPixelColor(a: Int, b: Int): Vec3 = {
		var sum = Vec3()
		for(i <- 0 until spp) {
			for(j <- 0 until spp) {
				val ray = Ray(camera.position, Vec3(a + (random + i)/spp, b + (random + j)/spp))
				val t = trace(ray)
				sum = sum + t
			}
		}
		sum*(1.0/(spp*spp))
	}

	def render(): Array[Array[Int]] = {
		Array.ofDim[Int](width, height)
			.map(_.map(_ => -1)).zipWithIndex
			.map(a => {
				new Array[Int](width).zipWithIndex.map(b => {
					val t = getPixelColor(a._2, height - b._2)
					val rgb = (((t.x*255).toInt & 0x0ff) << 16) | (((t.y*255).toInt & 0x0ff) << 8) | ((t.z*255).toInt & 0x0ff)
					rgb
				})
			})
	}

	def trace(r: Ray): Vec3
}



// For sampling a volume for volume rendering (smokey/transparent material)
//	@scala.annotation.tailrec
//	def sample(shape: Intersectable, ray: Ray): Ray = {
//		val tmax = shape.intersectDistance(ray)
//		val s = -0.01*Math.log(random)
//		if(s > tmax) {
//			Ray(ray.origin + ray.direction*(tmax+1), ray.origin + ray.direction*(tmax+2))
//		}
//		else {
//			val theta = random() * Pi
//			val phi = random() * 2 * Pi
//			val newDir = Vec3(Math.sin(theta) * Math.cos(phi), Math.sin(theta) * Math.sin(phi), Math.cos(theta))
//			val newRay = Ray(ray.origin + ray.direction * s, ray.origin + ray.direction * s + newDir)
//			sample(shape, newRay)
//		}
//	}