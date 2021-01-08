package invision.scene
import java.awt.image.BufferedImage

import invision.geometry.Ray
import invision.util.Vec3

import scala.math.random

/**
 * Handles the method by which a ray is traced and an image is rendered in the program.
 */
trait Renderer {
	/**
	 * The scene/world of objects.
	 */
	val world: World

	/**
	 * The camera of the scene. Determines position of primary ray.
	 */
	val camera: Camera

	/**
	 * Quantity of samples to take per pixel, squared.
	 */
	val spp: Int

	/**
	 * Width of output image.
	 */
	val width: Int

	/**
	 * Height of output image.
	 */
	val height: Int

	/**
	 * Determines how a ray will bounce around a scene to get the color associated with that light ray.
	 * @param r The ray being traced through the scene.
	 * @return A vector representing an rgb color.
	 */
	def trace(r: Ray): Vec3

	/**
	 * Gets the color of the pixel at a given position by sampling and averaging multiple rays traced through the scene.
	 * @param a Pixel's x position.
	 * @param b Pixel's y position.
	 * @return A vector representing an rgb color.
	 */
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

	/**
	 * Creates an image by going through each pixel and getting the pixel color.
	 * @return A 2D array representing an image.
	 */
	def render(): BufferedImage = {
		val im = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
		for(i <- 0 until width) {
			for(j <- 0 until height) {
				val t = getPixelColor(i, height - j)
				val rgb = (((t.x*255).toInt & 0x0ff) << 16) | (((t.y*255).toInt & 0x0ff) << 8) | ((t.z*255).toInt & 0x0ff)
				im.setRGB(i, j, rgb)
			}
		}
		im
	}
}

// For sampling a volume for volume rendering (smokey/transparent material)
//	@scala.annotation.tailrec
//	def sample(shape: Intersectable, ray: Ray): Ray = {
//		val tmax = shape.intersectDistance(ray)
//		val s = -0.01*Math.log(random)
//		if(s > tmax) {
//			Ray(ray.origin + ray.direction*(tmax+1), ray.origin + ray.direction*(tmax+2))
//		} else {
//			val newDir = Vec3.create(1, random() * Pi, random() * 2 * Pi)
//			val newRay = Ray(ray.origin + ray.direction * s, ray.origin + ray.direction * s + newDir)
//			sample(shape, newRay)
//		}
//	}