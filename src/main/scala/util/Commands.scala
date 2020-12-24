package raytracing.util;
import raytracing.{geometry,scene},geometry._,scene._
import annotation.tailrec;
import java.io.FileNotFoundException
import scala.collection.parallel.mutable.ParArray

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
	private def test(): Scene = {
		Scene(spp=SceneSettings.spp, width=SceneSettings.width, height=SceneSettings.height)++(
			BallLight(r=20,x=640,y=500,z=400)
		)++(
			Plane(Vec3(0,0,-1),Vec3(0,0,2000)),
			Gloss(2.0, 0.0),
			Vec3(1, 0, 0)
		)++(
			Plane(Vec3(1,0,0),Vec3(0,0,0)),
			Diffuse(1.0),
			Vec3(0, 1, 0)
		)++(
			Plane(Vec3(-1,0,0),Vec3(1280,0,0)),
			Diffuse(0.75),
			Vec3(0, 0, 1)
		)++(
			Plane(Vec3(0,1,0),Vec3(0,0,0)),
			Diffuse(0.5),
			Vec3(1, 0, 1)
		)++(
			Plane(Vec3(0,-1,0),Vec3(0,960,0)),
			Diffuse(0.25),
			Vec3(1, 1, 0)
		)++(
			SurfaceMarcher.Box(1280,200,1200).translate(640, 100, 800),
			Diffuse(1.0),
			Vec3(1,1,1)
		)++(
			SurfaceMarcher.Sphere(100).translate(200, 600, 300),
			Diffuse(20.0),
			Vec3(0,0,0)
		)
	}
	private val HELP = """
		|--------------- HELP MENU ---------------
		|               In progress
		|------------- END HELP MENU -------------
		""".stripMargin
	val render: (String, Scene) => Unit = (name: String, scene: Scene) => {
		val timer = new Timer();
		timer.start()
		val arr: ParArray[ParArray[Int]] = scene.render()
		timer.end()
		ImageHandler.saveImage(scene, arr, name)
		ImageHandler.saveData(name, scene, timer)
	}
	private def getIntInput(default: Int): Int = {
		try {
			scala.io.StdIn.readInt
		} catch {
			case e: Throwable => default
		}
	}
	private def getStringInput(default: String): String = {
		try {
			scala.io.StdIn.readLine
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
			case "c" =>
				ImageHandler.combine(getStringInput(""), getStringInput(""))
			case "1" =>
				println("Begun rendering")
				println(SceneSettings)
				render("test" + SceneSettings.spp, test())
			case "h" => println(HELP)
			case _ => println("invalid command")
		}
		val nextCommand = scala.io.StdIn.readLine;
		evaluate(nextCommand);
	}
	def evaluate(): Boolean = {
		println("[0] Set Scene Settings")
		println("[1] Render")
		println("[q] Quit")
		println("[h] Help")
		val command = scala.io.StdIn.readLine;
		evaluate(command);
	}
}