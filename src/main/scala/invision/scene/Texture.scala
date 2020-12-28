package invision.scene

import java.awt.image.BufferedImage

import invision.util.Vec3
import java.io.File

import javax.imageio.ImageIO

import scala.math.{Pi, asin, atan2}

/**
 * Texture for objects.
 * @param filename filename where the texture is located
 */
case class Texture(filename: String) {
	val picture: BufferedImage = ImageIO.read(new File(filename))

	def sphereUV(pt: Vec3, center: Vec3): Vec3 = {
		val d = (pt - center).normalize()
		val u = (0.5 + atan2(d.z, d.x)/(Pi*2))*picture.getWidth
		val v = (0.5 - asin(d.y)/Pi)*picture.getHeight
		Vec3(u, v)
	}
	
	def pixelColorAsVector(u: Double, v: Double): Vec3 = {
		val col = picture.getRGB(u.toInt, v.toInt)
		val red = (col & 0xff0000)/65536
		val green = (col & 0xff00)/256
		val blue = col & 0xff
		Vec3(red/255.0, green/255.0, blue/255.0)
	}
	
	def wrap(pt: Vec3, center: Vec3): Vec3 = {
		val uv = sphereUV(pt, center)
		pixelColorAsVector(uv.x, uv.y)
	}
	
	def paste(pt: Vec3, corner: Vec3, axis: Vec3): Vec3 = {
		val v = pt - corner
		val x = Math.abs(v*axis)%picture.getWidth
		val y = (v-axis*(v*axis)).magnitude%picture.getHeight
		pixelColorAsVector(x, y)
	}
}