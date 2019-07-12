/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,util},geometry.{Surface,Ray},util.{Timer,ImageHandler};
import scala.math.floor;

object Renderer {
	var columnsDone = 0;
	var paused = false;
	var current: Scene = null;
	var timer: Timer = null;
	var pixels: Array[Array[Int]] = null
	
	private def render(scene: Scene, column: Int): Array[Int] = {
		val arr = new Array[Int](scene.width);
		for(j <- 1 to scene.height) {
			val t = scene.getPixelColor(column, j-1);
			val rgb = (((t.x*255).toInt & 0x0ff) << 16) | (((t.y*255).toInt & 0x0ff) << 8) | ((t.z*255).toInt & 0x0ff);
			arr(scene.height-j) = rgb;
		}
		return arr;
	}
	def render(scene: Scene): Array[Array[Int]] = {
		pixels = Array.ofDim[Int](scene.width, scene.height).map(_.map(x => -1));
		current = scene;
		timer = new Timer()
		timer.start
		while(columnsDone < scene.width) {
			if(!paused) {
				pixels(columnsDone) = render(scene, columnsDone);
				columnsDone = columnsDone + 1;
			}
		}
		columnsDone = 0;
		current = null;
		timer = null;
		return pixels;
	}
	def getProgress(): Double = {
		if(current != null) {
			return floor((columnsDone/current.width.toDouble)*1000)/10.0;
		} else return 0.0
	}
	def getTimeSinceStart(): String = {
		if(timer != null) {
			return timer.formatTimeSinceStart
		} else return ""
	}
	def progressToString(): String = {
		if(timer != null) {
			return getProgress + "% -- " + getTimeSinceStart + " -- expected completion in (" +
				timer.formatTime(timer.getTimeSinceStart*(100-getProgress)/getProgress) + ")"
		} else return ""
	}
	def rasterize() {
		//TODO
	}
	def save() {
		ImageHandler.saveState(pixels);
	}
	//TODO
	// def load() {
		// val str = 
		// str.split("\n\r").map(_.split(",").map(_.toInt))
	// }
}