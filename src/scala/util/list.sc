/*
** Author:  Massimo Angelillo
** The random access time for an array is O(1)
** For a regular linked list it's O(n) where n is the size of the list
** For this linked list it should be O(log(n))
*/

package raytracing.util;

case class RAList[A](value: A, length: Int=0, left: RAList[A]=Empty, right: RAList[A]=Empty) {
	// def add(_val: A): RAList[A] = {
		
	// }
	def isEmpty(): Boolean = {
		return false;
	}
}

object Empty extends RAList[Any](0) {
	override def isEmpty(): Boolean = {
		return true;
	}
}