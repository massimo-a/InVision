import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object Commands {
	val commands = Array("load", "pause", "quit", "render", "save", "unpause");
	// def executeCommand(command: String): (String) => () = {
		// val i = commands.indexOf(command)
		// return i match {
			// case 0 => load
			// case 1 => pause
			// case 2 => quit
			// case 3 => render
			// case 4 => save
			// case 5 => unpause
			// case _ => error
		// }
	// }
}