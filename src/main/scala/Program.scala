/**
 * @author Massimo Angelillo
 *
 * A special thank you to Kevin Sangurima for assistance in
 * solving some algorithm issues, naming it and testing it out!
 */

import invision.util._

object Program {
	//metadata
	private val version = "v0.0.1"
	private val lineStart = "$>"
	private val programName = "InVision"
	
	//pretty println method
	private def p(s: String) {
		println(lineStart + " " + s)
	}
	
	def main(args: Array[String]): Unit = {
		p(programName + " booted up, " + version)
		Commands.evaluate()
		p("=== GOODBYE! ===")
	}
}