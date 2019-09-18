/*
** Author:  Massimo Angelillo
**
** This class will handle parsing and executing commands
** sent by the user
*/

package raytracing.util;
import raytracing.{geometry,scene},geometry._,scene._;
import scala.concurrent.{Future,ExecutionContext},ExecutionContext.Implicits.global;
import annotation.tailrec;
import java.io.FileNotFoundException

object Commands {
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
				p("Select sample size")
				val spp = scala.io.StdIn.readInt;
				p("Begun rendering empty room preset at " + spp*spp + " samples per pixel");
				render("empty_room_" + spp, Presets.emptyRoom(spp));
			}
			case "h" => p("sorry, in progress");
			case _ => p("invalid command");
		}
		val nextCommand = scala.io.StdIn.readLine;
		return evaluate(nextCommand);
	}
	def evaluate(): Boolean = {
		p("Select a preset scene")
		p("[0] Empty Room")
		p("[q] Quit")
		p("[h] Help")
		val command = scala.io.StdIn.readLine;
		return evaluate(command);
	}
}