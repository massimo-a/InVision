/**
 * @author Massimo Angelillo
 *
 * A special thank you to Kevin Sangurima for assistance in
 * solving some algorithm issues, naming it and testing it out!
 */

import java.awt.image.BufferedImage
import java.io.{File, FileNotFoundException, FileWriter}
import java.util.Calendar

import commandline.{NilValue, Option, Parser, Values}
import invision.geometry.intersectable.{Plane, SurfaceMarcher}
import invision.scene.material.{Diffuse, Gloss}
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
	private val pictureSaveLocation = "pictures/test pictures/"

	private val parser = Parser(List(
		Option(name = "--width", shortName = "-w", helpText = "Width of the image to be generated and also the viewport of the camera"),
		Option(name = "--height", shortName = "-e", helpText = "Height of the image to be generated and also the viewport of the camera"),
		Option(name = "--spp", shortName = "-s", helpText = "The samples per pixel (squared) to be taken. For example, an spp of 2 means 4 rays will be traced per pixel"),
		Option(name = "--help", shortName = "-h", helpText = "Print this help screen", numberOfArguments = 0),
		Option(name = "--fast", shortName = "-f", helpText = "Flag for fast rendering. Fast rendering ignores reflections, refractions and soft shadows"),
		Option(name = "--version", shortName = "-v", helpText = "Prints version information", numberOfArguments = 0)
	))

	private def testWorld(): World = {
		World()++
			BallLight(r=20,x=640,y=450,z=400)++(
			Plane(Vec3(0,0,-1),Vec3(0,0,2000)),
			Gloss(2.0),
			Vec3(1)
		)++(
			Plane(Vec3(0,1),Vec3()),
			Diffuse(0.5),
			Vec3(1, 0, 1)
		)++(
			Plane(Vec3(1),Vec3()),
			Diffuse(0.5),
			Vec3(0.5, 0, 1)
		)++(
			Plane(Vec3(-1),Vec3(1280)),
			Diffuse(0.5),
			Vec3(0.2, 0.2, 1)
		)++(
			Plane(Vec3(0,-1),Vec3(0,960)),
			Diffuse(0.25),
			Vec3(1, 1)
		)++(
			SurfaceMarcher.Box(1280,200,1200).translate(640, 100, 800),
			Diffuse(1.0),
			Vec3(1,1,1)
		)++(
			SurfaceMarcher.Sphere(100).translate(200, 600, 300),
			Diffuse(20.0),
			Vec3()
		)
	}

	private def logData(name: String, width: Int, height: Int, spp: Int, t: Double) {
		try {
			val pw = new FileWriter(pictureSaveLocation + "log.txt", new File(pictureSaveLocation + "log.txt").exists)
			pw.write(s"\r\ndate and time - ${Calendar.getInstance().getTime} \r\n")
			pw.write(s"file name - $name \r\n")
			pw.write(s"run time in seconds - $t \r\n")
			pw.write(s"anti-aliasing - $spp \r\n")
			pw.write(s"screen size - ($height, $width) \r\n")
			try {
				pw.write(s"primary rays shot per second - ${height*width*spp*spp/(t + 1E-12)} \r\n")
			} catch {
				case _: ArithmeticException =>
					pw.write("primary rays shot per second - TOO MANY \r\n")
			}
			pw.close()
		} catch {
			case _: FileNotFoundException => println("could not save data, file path not found")
		}
	}

	private def saveImage(name: String, width: Int, height: Int, spp: Int, rgbs: Array[Array[Int]]) {
		val im = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
		for(i <- 0 until width) {
			for(j <- 0 until height) {
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
							println (parser.usage () )
						case "-f" =>
							isFast = true
						case "-v" =>
							println (s"$programName Version $version")
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
		val arr: Array[Array[Int]] = renderer.render()
		val end = System.currentTimeMillis()
		saveImage("test" + spp, width, height, spp, arr)
		logData("test" + spp, width, height, spp, (end - start)/1000.0)
	}
}