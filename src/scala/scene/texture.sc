/*
** Author:  Massimo Angelillo
*/

package raytracing.scene

import raytracing.util.Vec3
import java.io.File
import javax.imageio.ImageIO

object Texture {
	def load(filename: String): Array[Array[Int]] = {
		val pic = ImageIO.read(new File(filename))
		return Array.tabulate(pic.getWidth, pic.getHeight)((i, j) => {
			pic.getRGB(i, j);
		})
	}
	def toVector(num: Int): Vec3 = {
		val red = (num & 0xff0000)/65536
		val green = (num & 0xff00)/256
		val blue = (num & 0xff)
		Vec3(red/255.0, green/255.0, blue/255.0);
	}
}