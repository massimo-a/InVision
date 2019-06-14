import raytracing.{geometry,scene,util},geometry._,scene._,util._;
import scala.concurrent.{Future,ExecutionContext},ExecutionContext.Implicits.global;

object Commands {
	def isQuitCommand(cmd: String): Boolean = {
		return (cmd.equals("q") || cmd.equals("quit"))
	}
	def isRenderCommand(cmd: String): Boolean = {
		return (cmd.equals("r") || cmd.equals("render"))
	}
	val pause = (name: String, scene: Scene) => {
		Renderer.paused = true
	}
	val unpause = (name: String, scene: Scene) => {
		Renderer.paused = false
	}
	val load = (name: String, scene: Scene) => {
		//TODO
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
}