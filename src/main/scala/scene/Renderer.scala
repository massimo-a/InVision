/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,util},geometry.{SurfaceMarcher,Ray},util.{Timer,ImageHandler};
import scala.math.floor;

object Renderer {
	var current: Scene = null
	private def render(scene: Scene, column: Int): Array[Int] = {
		val arr = new Array[Int](scene.width)
		for(j <- 1 to scene.height) {
			val t = scene.getPixelColor(column, j-1)
			val rgb = (((t.x*255).toInt & 0x0ff) << 16) | (((t.y*255).toInt & 0x0ff) << 8) | ((t.z*255).toInt & 0x0ff)
			arr(scene.height-j) = rgb
		}
		print("\r" + getProgress(column) + "%")
		return arr
	}
	def render(scene: Scene): Array[Array[Int]] = {
		val pixels = Array.ofDim[Int](scene.width, scene.height).map(_.map(x => -1))
		current = scene
		val timer = new Timer()
		timer.start
		for(columnsDone <- 0 until scene.width) {
			pixels(columnsDone) = render(scene, columnsDone)
		}
		println()
		current = null;
		return pixels;
	}
	def getProgress(col: Int): Double = {
		if(current != null) {
			return floor((col/current.width.toDouble)*1000)/10.0
		} else return 0.0
	}
}