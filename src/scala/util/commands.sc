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

object Commands {
	val load = (filename: String, name: String) => {
		val handler = new XMLHandler(filename);
		val scene = handler.load();
		render(name, scene);
	}
	//asynchronously renders an image
	val render = (name: String, scene: Scene) => {
		val timer = new Timer();
		val arr: Future[Array[Array[Int]]] = Future {
			timer.start;
			Renderer.render(scene);
		}
		arr.map { rgbs =>
			timer.end;
			ImageHandler.saveImage(scene, rgbs, name);
			println("Completed " + name);
			println("Run Time - " + timer.formatTime);
			ImageHandler.saveData(name, scene, timer);
		}
	}
	val save = (name: String, scene: Scene) => {
		//TODO
	}
	//pretty println method
	private def p(s: String) {
		println("$> " + s);
	}
	@tailrec private def evaluate(command: String): Boolean = {
		val commands = command.toLowerCase.split(" ");
		commands(0) match {
			case "q" => return false;
			case "r" => {
				p("Begun rendering " + commands(1));
				render(commands(1), SceneSetup.scene);
			}
			case "prog" => println(Renderer.progressToString);
			case "load" => {
				p("Begun rendering " + commands(1));
				load(commands(1), commands(2));
			}
			case "help" => p("sorry, in progress");
			case _ => p("invalid command");
		}
		val nextCommand = scala.io.StdIn.readLine;
		return evaluate(nextCommand);
	}
	def evaluate(): Boolean = {
		val command = scala.io.StdIn.readLine;
		return evaluate(command);
	}
}