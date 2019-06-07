/*
** Author:  Massimo Angelillo
** A special thank you to Kevin Sangurima for assisstance in
** solving some algorithm issues, naming it and testing it out!
** TRAC3R v0.2.7
*/

import raytracing.{geometry,scene,util},geometry._,scene._,util._;
import scala.concurrent.{Future,ExecutionContext},ExecutionContext.Implicits.global;
import annotation.tailrec;

object Program {
	//metadata
	private val version = "v0.2.7";
	private val lineStart = "$>";
	private val programName = "TRAC3R";
	
	//pretty println method
	private def p(s: String) {
		println(lineStart + " " + s);
	}
	
	//asynchronously renders an image
	private def render(name: String, scene: Scene) {
		val timer = new Timer();
		val arr: Future[Array[Array[Int]]] = Future {
			timer.start;
			Renderer.render(scene);
		}
		arr.map { rgbs =>
			timer.end;
			ImageHandler.saveImage(scene, rgbs, name);
			p("Completed " + name);
			p("Run Time - " + timer.formatTime);
			ImageHandler.saveData(name, scene, timer);
		}
	}
	@tailrec private def evaluate(command: String): Boolean = {
		val commands = command.toLowerCase.split(" ");
		val cmd = commands(0)(0).toString
		if(cmd.equals("q")) return false;
		if(cmd.equals("r")) {
			p("Begun rendering " + commands(1));
			render(commands(1), Scene()++(Lighting()));
		} else if(commands(0).equals("progress")) {
			p(Renderer.progressToString);
		} else if(cmd.equals("p")) {
			Renderer.paused = true;
		} else if(cmd.equals("u")) {
			Renderer.paused = false;
		} else if(cmd.equals("s")) {
			Renderer.save;
		} else p("invalid command");
		val nextCommand = scala.io.StdIn.readLine;
		return evaluate(nextCommand);
	}
	def evaluate(): Boolean = {
		val command = scala.io.StdIn.readLine;
		return evaluate(command);
	}
	def main(args: Array[String]): Unit = {
		p(programName + " booted up, " + version);
		evaluate;
		p("=== GOODBYE! ===");
	}
}