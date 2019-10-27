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
		var width: Int = 1000
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
	private def runPreset(i: String) {
		i match {
			case "1" => {
				println("Begun rendering")
				println(SceneSettings)
				render("test" + SceneSettings.spp, Presets.Test(SceneSettings.spp, SceneSettings.width, SceneSettings.height));
			}
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
			case "1" => {runPreset(command)}
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