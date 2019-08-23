/*
** Author:  Massimo Angelillo
**
** A special thank you to Kevin Sangurima for assisstance in
** solving some algorithm issues, naming it and testing it out!
** TRAC3R v0.4.0
*/

import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object Program {
	//metadata
	private val version = "v0.0.5";
	private val lineStart = "$>";
	private val programName = "TRAC3R";
	
	//pretty println method
	private def p(s: String) {
		println(lineStart + " " + s);
	}
	
	def main(args: Array[String]): Unit = {
		p(programName + " booted up, " + version);
		Commands.evaluate;
		p("=== GOODBYE! ===");
	}
}