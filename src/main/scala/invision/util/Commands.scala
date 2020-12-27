package invision.util

import invision.geometry._
import invision.geometry.intersectable.Plane
import invision.scene._
import invision.scene.material.{Diffuse, Gloss}
import invision.scene.renderable.BallLight

import scala.annotation.tailrec
import scala.math.tan

/** 
 *  @author Massimo Angelillo
 */
object Commands {
	private object SceneSettings {
		var spp: Int = 1
		var width: Int = 1280
		var height: Int = 960
		override def toString: String = {
			s"""
				|Settings: {
				|    Samples Per Pixel  : ${spp*spp}
				|    Screen Width       : $width
				|    Screen Height      : $height
				|    Total Rays Shot    : ${spp*spp*width*height}
				|}
			""".stripMargin
		}
	}

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

	val render: (String, Renderer) => Unit = (name: String, renderer: Renderer) => {
		val timer = new Timer()
		timer.start()
		val arr: Array[Array[Int]] = renderer.render()
		timer.end()
		ImageHandler.saveImage(renderer, arr, name)
		ImageHandler.saveData(name, renderer, timer)
	}
	private def getIntInput(default: Int): Int = {
		try {
			scala.io.StdIn.readInt
		} catch {
			case _: Throwable => default
		}
	}

	@tailrec private def evaluate(command: String): Boolean = {
		command match {
			case "q" => return false;
			case "0" =>
				println("Set samples per pixel")
				SceneSettings.spp = getIntInput(SceneSettings.spp)
				println("Set screen width")
				SceneSettings.width = getIntInput(SceneSettings.width)
				println("Set screen height")
				SceneSettings.height = getIntInput(SceneSettings.height)
			case "1" =>
				println("Begun rendering")
				println(SceneSettings)
				render("test" + SceneSettings.spp, RayTracer(testWorld(), camera = Camera(Vec3(SceneSettings.width/2.0, SceneSettings.height/2.0, -SceneSettings.width/(2*tan(Math.PI/8))), Math.PI/8), width=SceneSettings.width, height=SceneSettings.height, spp=SceneSettings.spp))
			case "h" => println("")
			case _ => println("invalid command")
		}
		val nextCommand = scala.io.StdIn.readLine
		evaluate(nextCommand)
	}

	def evaluate(): Boolean = {
		println("[0] Set Scene Settings")
		println("[1] Render")
		println("[q] Quit")
		println("[h] Help")
		val command = scala.io.StdIn.readLine
		evaluate(command)
	}
}