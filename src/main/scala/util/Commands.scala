/*
** Author:  Massimo Angelillo
**
** This class will handle parsing and executing commands
** sent by the user
*/

package raytracing.util;
import raytracing.{geometry,scene,server},geometry._,scene._,server.WebSocket
import annotation.tailrec;
import java.io.FileNotFoundException

object Commands {
	private object SceneSettings {
		var spp: Int = 1
		var width: Int = 500
		var height: Int = 500
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
		val no = Value("helloworld")
		val b = SurfaceMarcher.Sphere(30)
			.repeat(Vec3(120.0, 120.0, 120.0), 10)
			
		Scene(spp=SceneSettings.spp, width=SceneSettings.width, height=SceneSettings.height, up=Vec3(0, Math.cos((i/150.0)*Math.PI), -Math.sin((i/150.0)*Math.PI)).normalize)++(
			BallLight(r=30,x=500,y=900,z=200)
		)++(
			Plane(Vec3(0,0,-1),Vec3(0,0,1500)),
			Diffuse(),
			v => Vec3((((v.x-2000)/4000.0)%1.0 + 1.0)%1.0, (((v.y-2000)/4000.0)%1.0 + 1.0)%1.0, 0.5)
		)++(
			b,
			Diffuse(),
			Vec3(1, 0, 0)
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