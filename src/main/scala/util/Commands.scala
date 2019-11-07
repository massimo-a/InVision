/*
** Author:  Massimo Angelillo
**
** This class will handle parsing and executing commands
** sent by the user
*/

package raytracing.util;
import raytracing.{geometry,scene},geometry._,scene._
import annotation.tailrec;
import java.io.FileNotFoundException

object Commands {
	private object SceneSettings {
		var spp: Int = 1
		var width: Int = 1500
		var height: Int = 1000
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
	private def test(i: Int): Scene = {
		
		val cyl = SurfaceMarcher.Cylinder(100, 600)
			.subtract(SurfaceMarcher.Sphere(80).translate(0, 0, -100))
			.translate(700, 300, 1200)
		
		Scene(spp=SceneSettings.spp, width=SceneSettings.width, height=SceneSettings.height)++(
			BallLight(r=20,x=500,y=900,z=200)
		)++(
			Plane(Vec3(0,0,-1),Vec3(0,0,2000)),
			Gloss(),
			Vec3(1, 0, 0)
		)++(
			Plane(Vec3(1,0,0),Vec3(0,0,0)),
			Diffuse(0.0),
			Vec3(0, 1, 0)
		)++(
			Plane(Vec3(-1,0,0),Vec3(1500,0,0)),
			Diffuse(0.0),
			Vec3(0, 0, 1)
		)++(
			Plane(Vec3(0,1,0),Vec3(0,0,0)),
			Diffuse(0.0),
			Vec3(1, 0, 1)
		)++(
			Plane(Vec3(0,-1,0),Vec3(0,1000,0)),
			Diffuse(0.0),
			Vec3(1, 1, 0)
		)++(
			SurfaceMarcher.Sphere(300).translate(1200, 300, 1700),
			Diffuse(5.0),
			Vec3(0.75, 0.75, 0.75)
		)++(
			SurfaceMarcher.Box(200, 600, 200).rotateY(Math.PI/6).translate(400, 300, 1000),
			Gloss(1.0, 0.5),
			Vec3(0.75, 0.75, 0.75)
		)++(
			cyl,
			Gloss(1.0, 2.0),
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
		val arr: Array[Array[Int]] = Renderer.render(scene);
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
			case "1" => {
				println("Begun rendering")
				println(SceneSettings)
				for(i <- 0 until 1) {
					render("test" + i + "" + SceneSettings.spp, test(i));
				}
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