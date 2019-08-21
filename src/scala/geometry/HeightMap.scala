/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;

// a wrapper object for working with 2d arrays that represent heightmaps
object HeightMap {
	def generate(width: Int, depth: Int) = (func: (Int,Int)=>Double) => {
		Array.tabulate(width,depth)(func)
	}
	def add(arr1: Array[Array[Double]]) = (arr2: Array[Array[Double]]) => {
		generate(arr1.length,arr1(0).length)((i,j)=>(arr1(i)(j)+arr2(i)(j)))
	}
	def scale(arr: Array[Array[Double]]) = (sc: Double) => {
		arr.map(_.map(_*sc))
	}
}