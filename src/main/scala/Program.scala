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

import commandline.{NilValue, Option, Parser, Values}
import invision.geometry.intersectable.{Plane, SurfaceMarcher, Triangle}
import invision.scene.material.{Diffuse, Gloss, Transparent}
import invision.scene.renderable.BallLight
import invision.scene.{Camera, FastTracer, PathTracer, World}
import invision.util._
import javax.imageio.ImageIO

object Program {
	private val version = "v0.0.1"
	private val programName = "InVision"

	private var width = 1280
	private var height = 960
	private var spp = 1
	private var isFast = false
	private var pictureSaveLocation = "pictures\\test pictures\\"
	private var pictureName = "test"

	private val parser = Parser(List(
		Option(name = "--width", shortName = "-w", helpText = "Width of the image to be generated and also the viewport of the camera"),
		Option(name = "--height", shortName = "-e", helpText = "Height of the image to be generated and also the viewport of the camera"),
		Option(name = "--spp", shortName = "-s", helpText = "The samples per pixel (squared) to be taken. For example, an spp of 2 means 4 rays will be traced per pixel"),
		Option(name = "--help", shortName = "-h", helpText = "Print this help screen", numberOfArguments = 0),
		Option(name = "--fast", shortName = "-f", helpText = "Flag for fast rendering. Fast rendering ignores reflections, refractions and soft shadows", numberOfArguments = 0),
		Option(name = "--version", shortName = "-v", helpText = "Prints version information", numberOfArguments = 0),
		Option(name = "--save-to", shortName = "-t", helpText = "File location where image will be saved. Defaults to '~\\pictures\\test pictures'"),
		Option(name = "--save-as", shortName = "-a", helpText = "Name of image file. Defaults to test_[spp value]")
	))

	private def testWorld(): World = {
		World()++
			BallLight(size=25,x=200,y=880,z=500)++
			(Triangle(vertex1=Vec3(),vertex2=Vec3(1280),vertex3=Vec3(1280,960,2000)),
				Transparent(2.0, 0.75),
				Vec3())++
			(Plane(Vec3(0,0,-1),Vec3(0,0,2000)),
				Diffuse(),
				Vec3(1))++
			(Plane(Vec3(0,1),Vec3()),
				Diffuse(0.5),
				Vec3(1, 0, 1))++
			(Plane(Vec3(1),Vec3()),
				Gloss(1.0, 0.4),
				Vec3(0.9, 0.2, 0.2))++
			(Plane(Vec3(-1),Vec3(1280)),
				Diffuse(0.5),
				Vec3(0.2, 0.2, 1))++
			(Plane(Vec3(0,-1),Vec3(0,960)),
				Diffuse(0.25),
				Vec3(1, 1))++
			(SurfaceMarcher.Sphere(100).translate(600, 200, 1900),
				Diffuse(),
				Vec3(0.5, 1.0, 0.1))
	}

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
							pictureSaveLocation = values.head.toString
						case "-a" =>
							pictureName = values.head.toString
					}
					case NilValue =>
				}
			})

		val renderer = if(isFast) {
			FastTracer(testWorld(), camera = Camera(width, height, Math.PI/8.0), width=width, height=height, spp=spp)
		} else {
			PathTracer(testWorld(), camera = Camera(width, height, Math.PI/8.0), width=width, height=height, spp=spp)
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