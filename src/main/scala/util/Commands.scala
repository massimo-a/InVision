/*
** Author:  Massimo Angelillo
**
** This class will handle parsing and executing commands
** sent by the user
*/

package raytracing.util;
import raytracing.{geometry,scene},geometry._,scene._;
import annotation.tailrec;
import java.io.FileNotFoundException

object Commands {
	private object SceneSettings {
		var spp: Int = 1
		var width: Int = 1000
		var height: Int = 1000
		override def toString(): String = {
			return "Settings:\n{\n" +
				"  Samples Per Pixel : " + spp*spp + "\n" +
				"  Screen Width : " + width + "\n" +
				"  Screen Height : " + height + "\n" +
				"}"
		}
	}
	
	private val HELP = "--------------- HELP MENU --------------- \n" + 
	"In progress \n" + 
	"------------- END HELP MENU -------------"
	
	val render = (name: String, scene: Scene) => {
		val timer = new Timer();
		timer.start;
		val arr: Array[Array[Int]] = Renderer.render(scene);
		timer.end;
		ImageHandler.saveImage(scene, arr, name);
		ImageHandler.saveData(name, scene, timer);
	}
	
	//pretty println method
	private def p(s: String) {
		println("$> " + s);
	}
	@tailrec private def evaluate(command: String): Boolean = {
		command match {
			case "q" => return false;
			case "0" => {
				p("Set samples per pixel")
				SceneSettings.spp = scala.io.StdIn.readInt
				p("Set screen width")
				SceneSettings.width = scala.io.StdIn.readInt
				p("Set screen height")
				SceneSettings.height = scala.io.StdIn.readInt
			}
			case "1" => {
				p("Begun rendering empty room preset")
				println(SceneSettings)
				render("empty_room_" + SceneSettings.spp, Presets.emptyRoom(SceneSettings.spp, SceneSettings.width, SceneSettings.height));
			}
			case "2" => {
				p("Begun rendering cornell box preset")
				println(SceneSettings)
				render("cornell_box_" + SceneSettings.spp, Presets.cornellBox(SceneSettings.spp, SceneSettings.width, SceneSettings.height));
			}
			case "3" => {
				p("Begun rendering small preset")
				println(SceneSettings)
				render("small" + SceneSettings.spp, Presets.small(SceneSettings.spp, SceneSettings.width, SceneSettings.height));
			}
			case "h" => println(HELP);
			case _ => p("invalid command");
		}
		val nextCommand = scala.io.StdIn.readLine;
		return evaluate(nextCommand);
	}
	def evaluate(): Boolean = {
		p("Select a preset scene")
		p("[0] Set Scene Settings")
		p("[1] Empty Room")
		p("[2] Cornell Box")
		p("[3] Small")
		p("[q] Quit")
		p("[h] Help")
		val command = scala.io.StdIn.readLine;
		return evaluate(command);
	}
}