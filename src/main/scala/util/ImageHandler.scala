/*
** Author:  Massimo Angelillo
**
** A utility class for handling Image I/O operations
** and certain other impure operations
*/

package raytracing.util;
import raytracing.{scene,geometry},scene.{Scene, Material},geometry._;
import javax.imageio.ImageIO;
import java.{io,util},io.{File,FileWriter},util.Calendar
import java.awt.image.BufferedImage

object ImageHandler {
	private val pictureSaveLocation = "C:\\Users\\Massimo Due\\Documents\\GitHub\\TRAC3R\\src\\pictures\\test pictures\\";
	def saveData(name: String, scene: Scene, t: Timer) {
		val pw = new FileWriter(pictureSaveLocation + "log.txt", new File(pictureSaveLocation + "log.txt").exists);
		pw.write("\r\ndate and time - " + Calendar.getInstance().getTime() + "\r\n");
		pw.write("file name - " + name + "\r\n");
		pw.write("run time - " + t.formatTime + "\r\n");
		pw.write("anti-aliasing - " + scene.spp + "\r\n");
		pw.write("screen size - (" + scene.height + ", " + scene.width + ")\r\n");
		pw.write("primary rays shot per second - " + scene.height*scene.width*scene.spp*scene.spp/t.getRunTime + "\r\n");
		pw.close
	}
	def saveImage(scene: Scene, rgbs: Array[Array[Int]], name: String) {
		val im = new BufferedImage(scene.width, scene.height, BufferedImage.TYPE_INT_RGB);
		for(i <- 0 until scene.width) {
			for(j <- 0 until scene.height) {
				im.setRGB(i, j, rgbs(i)(j))
			}
		}
		ImageIO.write(im, "png", new File(pictureSaveLocation + name + ".png"));
	}
	def saveState(arr: Array[Array[Int]]) {
		var str = arr.map(_.mkString(",")).mkString("\n\r")
		val pw = new FileWriter(pictureSaveLocation + "saved.txt", new File(pictureSaveLocation + "saved.txt").exists);
		pw.write(str)
		pw.close
	}
}