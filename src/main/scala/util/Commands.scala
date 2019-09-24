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
				"  Samples Per Pixel  : " + spp*spp + "\n" +
				"  Screen Width       : " + width + "\n" +
				"  Screen Height      : " + height + "\n" +
				"  Total Rays Shot    : " + spp*spp*width*height + "\n" +
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
				p("Begun rendering 'lots of shapes' preset")
				println(SceneSettings)
				render("lots_of_shapes" + SceneSettings.spp, Presets.lotsOfShapes(SceneSettings.spp, SceneSettings.width, SceneSettings.height));
			}
			case "4" => {
				p("Begun rendering pokeball preset")
				println(SceneSettings)
				render("pokeball" + SceneSettings.spp, Presets.pokeball(SceneSettings.spp, SceneSettings.width, SceneSettings.height));
			}
			case "5" => {
				p("Begun rendering open pokeball preset")
				println(SceneSettings)
				render("open_pokeball" + SceneSettings.spp, Presets.pokeballOpen(SceneSettings.spp, SceneSettings.width, SceneSettings.height));
			}
		}
	}
	@tailrec private def evaluate(command: String): Boolean = {
		command match {
			case "q" => return false;
			case "0" => {
				p("Set samples per pixel")
				SceneSettings.spp = getIntInput(SceneSettings.spp)
				p("Set screen width")
				SceneSettings.width = getIntInput(SceneSettings.width)
				p("Set screen height")
				SceneSettings.height = getIntInput(SceneSettings.height)
			}
			case "1"|"2"|"3"|"4"|"5" => {runPreset(command)}
			case "h" => println(HELP);
			case _ => p("invalid command");
		}
		val nextCommand = scala.io.StdIn.readLine;
		return evaluate(nextCommand);
	}
	def evaluate(): Boolean = {
		p("[0] Set Scene Settings")
		p("[1] Empty Room")
		p("[2] Cornell Box")
		p("[3] Lots of Shapes")
		p("[4] A pokeball!")
		p("[5] An open pokeball!!")
		p("[q] Quit")
		p("[h] Help")
		val command = scala.io.StdIn.readLine;
		return evaluate(command);
	}
}