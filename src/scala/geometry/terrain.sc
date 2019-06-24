/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;

case class Terrain(
	heightmap: Array[Array[Double]],
	height: Double
) extends Surface with Bounded {
	val width = heightmap.length;
	val depth = heightmap(0).length
	val OOBB = Array(
		-Vec3(width/2,height/2,depth/2),Vec3(width/2,-height/2,-depth/2),
		Vec3(width/2,-height/2,depth/2),Vec3(-width/2,-height/2,depth/2),
		Vec3(-width/2,height/2,-depth/2),Vec3(width/2,height/2,-depth/2),
		Vec3(width/2,height/2,depth/2),Vec3(-width/2,height/2,depth/2)
	)
	val minimum = Vec3(-width,-height,-depth)
	val maximum = Vec3(width,height,depth)

	val equation = (vec: Vec3) => {
		val x = ((vec.x.toInt%width + width)%width).toInt
		val z = ((vec.z.toInt%depth + depth)%depth).toInt
		vec.y - heightmap(x)(z)
	}
	override def gradient(pt: Vec3): Vec3 = {
		val grad_x = (equation(pt)-equation(pt - Vec3(x=1)));
		val grad_y = (equation(pt)-equation(pt - Vec3(y=1)));
		val grad_z = (equation(pt)-equation(pt - Vec3(z=1)));
		return Vec3(grad_x, grad_y, grad_z);
	}
	def intersectDistance(r: Ray): Double = {
		if(hitBox(r)) {
			var inter = intersections(r)
			var pt = inter._1
			while(pt < inter._2) {
				if(Math.abs(equation(r.equation(pt))) < 1) return pt
				pt = pt + 1/Math.abs(r.direction.z)
			}
		}
		return -1
	}
}
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