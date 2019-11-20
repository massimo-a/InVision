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
		override def toString(): String = {
			return s"""
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
		
		val cyl = SurfaceMarcher.Cylinder(100, 600)
			.subtract(SurfaceMarcher.Sphere(80).translate(0, -200, -100))
			.subtract(SurfaceMarcher.Sphere(80).translate(0, 0, -100))
			.subtract(SurfaceMarcher.Sphere(80).translate(0, 200, -100))
			.translate(200, 300, 1600)
		
		val hm = Array.ofDim[Double](1000, 1000)
		val t = Terrain(hm, 200)
		
		var scene = Scene(spp=SceneSettings.spp, width=SceneSettings.width, height=SceneSettings.height)
		
		scene++(
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
			SurfaceMarcher.Sphere(100).translate(1160, 300, 800),
			Transparent(1.0, 0.0),
			Vec3(1,0,0)
		)++(
			SurfaceMarcher.Sphere(100).translate(640, 300, 800),
			Transparent(1.0, 0.0),
			Vec3(0,1,0)
		)++(
			SurfaceMarcher.Sphere(100).translate(120, 300, 800),
			Transparent(1.0, 0.0),
			Vec3(1,1,0)
		)++(
			cyl,
			Diffuse(0.0),
			Vec3(0.75, 0.75, 0.75)
		)
	}
	private val HELP = """
		|--------------- HELP MENU ---------------
		|               In progress
		|------------- END HELP MENU -------------
		""".stripMargin
	val render = (name: String, scene: Scene) => {
		val timer = new Timer();
		timer.start;
		val arr: ParArray[ParArray[Int]] = scene.render();
		timer.end;
		ImageHandler.saveImage(scene, arr, name);
		ImageHandler.saveData(name, scene, timer);
	}
	private def getIntInput(default: Int): Int = {
		return try {
			scala.io.StdIn.readInt
		} catch {
			case e: Throwable => default
		}
	}
	private def getStringInput(default: String): String = {
		return try {
			scala.io.StdIn.readLine
		} catch {
			case e: Throwable => default
		}
	}
	@tailrec private def evaluate(command: String): Boolean = {
		command match {
			case "q" => return false;
			case "0" => {
				println("Set samples per pixel")
				SceneSettings.spp = getIntInput(SceneSettings.spp)
				println("Set screen width")
				SceneSettings.width = getIntInput(SceneSettings.width)
				println("Set screen height")
				SceneSettings.height = getIntInput(SceneSettings.height)
			}
			case "c" => {
				ImageHandler.combine(getStringInput(""), getStringInput(""))
			}
			case "1" => {
				println("Begun rendering")
				println(SceneSettings)
				render("test" + SceneSettings.spp, test())
			}
			case "h" => println(HELP)
			case _ => println("invalid command")
		}
		val nextCommand = scala.io.StdIn.readLine;
		return evaluate(nextCommand);
	}
	def evaluate(): Boolean = {
		println("[0] Set Scene Settings")
		println("[1] Render")
		println("[q] Quit")
		println("[h] Help")
		val command = scala.io.StdIn.readLine;
		return evaluate(command);
	}
}