package invision.util

import java.awt.image.BufferedImage
import java.io.{File, FileNotFoundException, FileWriter}
import java.util.Calendar

import invision.scene.Renderer
import javax.imageio.ImageIO

/** 
 *  @author Massimo Angelillo
 */
object ImageHandler {
	private val pictureSaveLocation = "src/pictures/test pictures/"

	def saveData(name: String, scene: Renderer, t: Timer) {
		try {
			val pw = new FileWriter(pictureSaveLocation + "log.txt", new File(pictureSaveLocation + "log.txt").exists)
			pw.write("\r\ndate and time - " + Calendar.getInstance().getTime + "\r\n")
			pw.write("file name - " + name + "\r\n")
			pw.write("run time - " + t.formatRunTime + "\r\n")
			pw.write("anti-aliasing - " + scene.spp + "\r\n")
			pw.write("screen size - (" + scene.height + ", " + scene.width + ")\r\n")
			try {
				pw.write("primary rays shot per second - " + scene.height*scene.width*scene.spp*scene.spp/(t.getRunTime + 1E-12) + "\r\n")
			} catch {
				case _: ArithmeticException =>
					pw.write("primary rays shot per second - TOO MANY \r\n")
			}
			pw.close()
			println("Runtime: " + t.formatRunTime)
		} catch {
			case _: FileNotFoundException => println("could not save data, file path not found")
		}
	}
	def saveImage(scene: Renderer, rgbs: Array[Array[Int]], name: String) {
		val im = new BufferedImage(scene.width, scene.height, BufferedImage.TYPE_INT_RGB)
		for(i <- 0 until scene.width) {
			for(j <- 0 until scene.height) {
				im.setRGB(i, j, rgbs(i)(j))
			}
		}
		try {
			ImageIO.write(im, "png", new File(pictureSaveLocation + name + ".png"))
			println("Completed: " + name)
		} catch {
			case _: FileNotFoundException => println("could not save picture, file path not found")
		}
	}
}