/*
** Author:  Massimo Angelillo
*/

package raytracing.util;
import raytracing.{geometry,scene},geometry._,scene._;
import annotation.tailrec;
import java.io.FileNotFoundException
import scala.io.Source



object Parser {
	var tokens: Token = EOF();
	var scene: Scene = null;
	val shapes = Array("sphere", "box", "torus", "cylinder")
	val ops = Array("smooth", "union", "intersect", "diff", "rotateX", "rotateY", "rotateZ")
	
	def loadFile(filename: String) {
		tokens = Token.tokenizeFile(filename)
	}
	
	def parse() {}
}