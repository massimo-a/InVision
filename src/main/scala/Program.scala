/**
 * @author Massimo Angelillo
 *
 * A special thank you to Kevin Sangurima for assistance in
 * solving some algorithm issues, naming it and testing it out!
 */

import java.awt.image.BufferedImage
import java.io.{File, FileNotFoundException, FileWriter}
import java.text.SimpleDateFormat
import java.util.Calendar

import commandline.{CommandLine, NilValue, Option, Values}
import invision.scene.{Camera, FastTracer, PathTracer, World}
import javax.imageio.ImageIO
import parser.SceneBuilder

object Program {
	private val version = "v0.0.1"
	private val programName = "InVision"
	private val sceneBuilder = SceneBuilder()

	private var width = 1280
	private var height = 960
	private var world = World()
	private var spp = 1
	private var isFast = false
	private var pictureSaveLocation = "pictures\\test pictures\\"
	private var pictureName = "test"

	private val parser = CommandLine(List(
		Option(name = "--width", shortName = "-w", helpText = "Width of the image to be generated and also the viewport of the camera"),
		Option(name = "--height", shortName = "-e", helpText = "Height of the image to be generated and also the viewport of the camera"),
		Option(name = "--spp", shortName = "-s", helpText = "The samples per pixel (squared) to be taken. For example, an spp of 2 means 4 rays will be traced per pixel"),
		Option(name = "--help", shortName = "-h", helpText = "Print this help screen", numberOfArguments = 0),
		Option(name = "--fast", shortName = "-f", helpText = "Flag for fast rendering. Fast rendering ignores reflections, refractions and soft shadows", numberOfArguments = 0),
		Option(name = "--version", shortName = "-v", helpText = "Prints version information", numberOfArguments = 0),
		Option(name = "--save-to", shortName = "-t", helpText = "File location where image will be saved. Defaults to '~\\pictures\\test pictures'"),
		Option(name = "--save-as", shortName = "-a", helpText = "Name of image file. Defaults to test_[spp value]"),
		Option(name = "--config", shortName = "-c", helpText = "Location of a config file for your scene")
	))

	private def logData(t: Double) {
		val format = new SimpleDateFormat("d_M_y")
		val correctPath = if (pictureSaveLocation.endsWith("\\")) "" else "\\"
		val logPath = s"$pictureSaveLocation${correctPath}LOG_${format.format(Calendar.getInstance().getTime)}.log"
		val log = new File(logPath)
		try {
			if (!log.exists) {
				log.createNewFile()
			}
			val pw = new FileWriter(log, true)
			pw.write(s"file name - ${pictureName}_$spp \r\n")
			pw.write(s"run time in seconds - $t \r\n")
			pw.write(s"anti-aliasing - $spp \r\n")
			pw.write(s"screen size - ($height, $width) \r\n")
			try {
				pw.write(s"primary rays shot per second - ${height * width * spp * spp / t} \r\n")
			} catch {
				case _: ArithmeticException =>
					pw.write("primary rays shot per second - TOO MANY \r\n")
			}
			pw.close()
		} catch {
			case _: FileNotFoundException => println("Could not save picture, file path not found")
		}
	}

	private def saveImage(im: BufferedImage) {
		try {
			val correctPath = if (pictureSaveLocation.endsWith("\\") || pictureName.head == '\\') "" else "\\"
			ImageIO.write(im, "png", new File(s"$pictureSaveLocation$correctPath${pictureName}_$spp.png"))
			println(s"Completed: $pictureSaveLocation$correctPath${pictureName}_$spp")
		} catch {
			case _: FileNotFoundException => println("Could not save picture, file path not found")
		}
	}

	def main(args: Array[String]): Unit = {
		parser.parse(args.toList)
			.withParsed(x => {
				x.values match {
					case Values(values) => x.shortName match {
						case "-w" =>
							width = values.head.toInt
						case "-e" =>
							height = values.head.toInt
						case "-s" =>
							spp = values.head.toInt
						case "-h" =>
							println(parser.usage())
						case "-f" =>
							isFast = true
						case "-v" =>
							println(s"$programName Version $version")
						case "-t" =>
							pictureSaveLocation = values.head
						case "-a" =>
							pictureName = values.head
						case "-c" =>
							world = sceneBuilder.load(values.head)
					}
					case NilValue =>
				}
			})

		val renderer = if(isFast) {
			FastTracer(world, camera = Camera(width, height, Math.PI/8.0), width=width, height=height, spp=spp)
		} else {
			PathTracer(world, camera = Camera(width, height, Math.PI/8.0), width=width, height=height, spp=spp)
		}

		val start = System.currentTimeMillis()
		println("Begun Rendering")
		println(s"Start Time: ${Calendar.getInstance().getTime}")
		val arr: BufferedImage = renderer.render()
		val end = System.currentTimeMillis()
		saveImage(arr)
		println(s"Completion Time: ${(end - start)/1000.0} seconds")
		logData((end - start)/1000.0)
	}
}