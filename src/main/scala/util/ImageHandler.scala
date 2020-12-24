package raytracing.util;
import raytracing.{scene,geometry},scene.{Scene, Material},geometry._;
import javax.imageio.ImageIO;
import java.{io,util},io.{File,FileWriter,FileNotFoundException},util.Calendar
import java.awt.image.BufferedImage
import java.awt.Color
import scala.collection.parallel.mutable.ParArray

/** 
 *  @author Massimo Angelillo
 */
object ImageHandler {
	private val pictureSaveLocation = "src/pictures/test pictures/";
	def saveData(name: String, scene: Scene, t: Timer) {
		try {
			val pw = new FileWriter(pictureSaveLocation + "log.txt", new File(pictureSaveLocation + "log.txt").exists);
			pw.write("\r\ndate and time - " + Calendar.getInstance().getTime + "\r\n");
			pw.write("file name - " + name + "\r\n");
			pw.write("run time - " + t.formatRunTime + "\r\n");
			pw.write("anti-aliasing - " + scene.spp + "\r\n");
			pw.write("screen size - (" + scene.height + ", " + scene.width + ")\r\n");
			try {
				pw.write("primary rays shot per second - " + scene.height*scene.width*scene.spp*scene.spp/(t.getRunTime + 1E-12) + "\r\n");
			} catch {
				case _: ArithmeticException =>
					pw.write("primary rays shot per second - TOO MANY \r\n")
			}
			pw.close()
			println("Runtime: " + t.formatRunTime);
		} catch {
			case _: FileNotFoundException => println("could not save data, file path not found")
		}
	}
	def saveImage(scene: Scene, rgbs: ParArray[ParArray[Int]], name: String) {
		val im = new BufferedImage(scene.width, scene.height, BufferedImage.TYPE_INT_RGB);
		for(i <- 0 until scene.width) {
			for(j <- 0 until scene.height) {
				im.setRGB(i, j, rgbs(i)(j))
			}
		}
		try {
			ImageIO.write(im, "png", new File(pictureSaveLocation + name + ".png"));
			println("Completed: " + name);
		} catch {
			case _: FileNotFoundException => println("could not save picture, file path not found")
		}
	}
	def difference(file1: String, file2: String) {
		var img1: BufferedImage = null;
		var img2: BufferedImage = null;
		try {
			img1 = ImageIO.read(new File(pictureSaveLocation + file1))
			img2 = ImageIO.read(new File(pictureSaveLocation + file2))
		} catch {
			case _: Throwable => println()
		}
		val img: BufferedImage = new BufferedImage(img1.getWidth(), img1.getHeight(), BufferedImage.TYPE_INT_RGB)
		for(i <- 0 until img.getWidth()) {
			for(j <- 0 until img.getHeight()) {
				img.setRGB(i, j, Math.abs(img1.getRGB(i, j) - img2.getRGB(i, j)))
			}
		}
		ImageIO.write(img, "png", new File(pictureSaveLocation + "difference.png"));
	}
	def combine(file1: String, file2: String) {
		var img1: BufferedImage = null;
		var img2: BufferedImage = null;
		try {
			img1 = ImageIO.read(new File(pictureSaveLocation + file1))
			img2 = ImageIO.read(new File(pictureSaveLocation + file2))
		} catch {
			case _: Throwable => println()
		}
		val img: BufferedImage = new BufferedImage(img1.getWidth(), img1.getHeight(), BufferedImage.TYPE_INT_RGB)
		val weight = 0.5
		for(i <- 0 until img.getWidth()) {
			for(j <- 0 until img.getHeight()) {
				val c1 = new Color(img1.getRGB(i, j))
				val c2 = new Color(img2.getRGB(i, j))
				val r = c1.getRed*weight + c2.getRed*(1-weight)
				val g = c1.getGreen*weight + c2.getGreen*(1-weight)
				val b = c1.getBlue*weight + c2.getBlue*(1-weight)
				img.setRGB(i, j, ((r.toInt & 0x0ff) << 16) | ((g.toInt & 0x0ff) << 8) | (b.toInt & 0x0ff))
			}
		}
		ImageIO.write(img, "png", new File(pictureSaveLocation + "combined.png"));
	}
}