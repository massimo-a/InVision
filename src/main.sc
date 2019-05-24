/*
 * Author: Massimo Angelillo
 * A special thank you to Kevin Sangurima for assisstance in
 * solving some algorithm issues, naming it and testing it out!
 * TRAC3R v0.2.7
*/

import raytracing.{geometry,scene,util},geometry._,scene._,util._;
import scala.concurrent.{Future,ExecutionContext},ExecutionContext.Implicits.global;
import annotation.tailrec;

object Commands {
	val commands = Array("load", "pause", "quit", "render", "save", "unpause");
}

object Room {
	
}

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
	private def window(width: Double, height: Double, gap: Double): Surface = {
		val box1 = Surface.BOX(Vec3(width, height, gap));
		val box2 = Surface.BOX(Vec3(width-gap, height-gap, gap+2));
		return box1.subtract(box2);
	}
	private def mug(r: Double, h: Double, g: Double): Surface = {
		val handle = (a: Double, b: Double) => {
			Surface.TORUS(a, b).
			rotateX(Math.PI/2).
			intersect(Surface.BOX(Vec3(a+b,a+b,b)).translate(a+b,0,0))
		}
		val body = (width: Double, height: Double, gap: Double) => {
			Surface.CYLINDER(width, height).
			subtract(Surface.CYLINDER(width-gap, height-gap).translate(0,gap+2,0))
		}
		return body(r, h, g).smoothUnion(handle(h/1.5, g).translate(r, 0, 0), 10);
	}
	private def setup(): Scene = {
		val scene = Scene(spp=8,showLight=true).
		++(Lighting(x=100, y=900, z=100, size=50)).
		++(Surface.BOX(Vec3(1000,50,3000)).stretchBoundingBox(Vec3(50,50,50)).distort((p: Vec3) =>{20*(Math.sin(p.z/50)-1)}).translate(500,0,1500), Gloss(0.75,0.3,0.3,0)).
		++(Surface.BOX(Vec3(1000,2,3000)).translate(500, 1000, 1500), Diffuse(0.85,0.85,0.85)).
		++(Surface.BOX(Vec3(2,1000,3000)).translate(0, 500, 1500), Diffuse(0.85,0.85,0.85)).
		++(Surface.BOX(Vec3(2,1000,3000)).translate(1000, 500, 1500), Diffuse(0.85,0.85,0.85)).
		++(Surface.BOX(Vec3(1000,1000,2)).translate(500, 500, 3000), Diffuse(0.85,0.85,0.85)).
		++(Surface.ELLIPSOID(Vec3(300,150,150)).stretchBoundingBox(Vec3(50,50,50)).distort((p: Vec3) =>{20*(Math.sin(p.x/40)-1)}).translate(500,500,1200), Gloss(0.75,0.75,0.75,0))
		return scene;
	}
	@tailrec def evaluate(command: String): Boolean = {
		val commands = command.toLowerCase.split(" ") ++ Array(" ");
		val cmd = commands(0)(0).toString
		if(cmd.equals("q")) return false;
		if(cmd.equals("r")) {
			p("Begun rendering " + commands(1));
			render(commands(1), setup());
		} else if(commands(0).equals("progress")) {
			p(Renderer.getProgress + "% -- " + Renderer.getTimeSinceStart);
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
		return evaluate("a");
	}
	def main(args: Array[String]): Unit = {
		p(programName + " booted up, " + version);
		evaluate;
		p("=== GOODBYE! ===");
	}
}