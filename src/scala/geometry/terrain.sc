/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.{Vec3,Noise};

case class Terrain(
	width: Double,
	depth: Double,
	height: Double,
	noise: Noise = Noise()
) extends Surface with Bounded {
	val OOBB = Array(
		-Vec3(width/2,height/2,depth/2),Vec3(width/2,-height/2,-depth/2),
		Vec3(width/2,-height/2,depth/2),Vec3(-width/2,-height/2,depth/2),
		Vec3(-width/2,height/2,-depth/2),Vec3(width/2,height/2,-depth/2),
		Vec3(width/2,height/2,depth/2),Vec3(-width/2,height/2,depth/2)
	)
	val minimum = Vec3(0,0,0)
	val maximum = Vec3(width,height,depth)
	
	private val terrain = Array.tabulate(width.toInt,depth.toInt) {(i,j) => noise.layered(i/200.0,j/200.0,10)*height}
	val equation = (vec: Vec3) => {
		val x = ((vec.x.toInt%width + width)%width).toInt
		val z = ((vec.z.toInt%depth + depth)%depth).toInt
		vec.y - terrain(x)(z)
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
			var pt = inter._1/2.0
			while(pt < inter._2) {
				if(Math.abs(equation(r.equation(pt))) < 5) return pt
				pt = pt + 1/Math.abs(r.direction.z)
			}
		}
		return -1
	}
}