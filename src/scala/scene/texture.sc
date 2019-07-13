/*
** Author:  Massimo Angelillo
*/

package raytracing.scene

import raytracing.util.Vec3
import java.io.File
import javax.imageio.ImageIO
import scala.math.{atan2,asin,Pi}

case class Texture(filename: String) {
	val picture = ImageIO.read(new File(filename));
	
	def sphereUV(pt: Vec3, center: Vec3): Vec3 = {
		val d = (pt - center).normalize
		val u = (0.5 + atan2(d.z, d.x)/(Pi*2))*picture.getWidth
		val v = (0.5 - asin(d.y)/Pi)*picture.getHeight
		return Vec3(u, v);
	}
	
	def pixelColorAsVector(u: Double, v: Double): Vec3 = {
		val col = picture.getRGB(u.toInt, v.toInt);
		val red = (col & 0xff0000)/65536
		val green = (col & 0xff00)/256
		val blue = (col & 0xff)
		return Vec3(red/255.0, green/255.0, blue/255.0)
	}
	
	def wrap(pt: Vec3, center: Vec3): Vec3 = {
		val uv = sphereUV(pt, center);
		return pixelColorAsVector(uv.x, uv.y)
	}
	
	def paste(pt: Vec3, corner: Vec3, axis: Vec3, width: Int, height: Int): Vec3 = {
		val v = pt - corner;
		val x = v.proj(axis).magnitude%width;
		val y = v.rej(axis).magnitude%height;
		return pixelColorAsVector(x, y);
	}
}