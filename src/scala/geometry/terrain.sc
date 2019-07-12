/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;

case class Terrain(
	heightmap: Array[Array[Double]],
	height: Double,
	x:Double=0,y:Double=0,z:Double=0
) extends SurfaceMarcher with Bounded {
	val width = heightmap.length;
	val depth = heightmap(0).length
	val OOBB = Array(
		-Vec3(width/2,height/2,depth/2),Vec3(width/2,-height/2,-depth/2),
		Vec3(width/2,-height/2,depth/2),Vec3(-width/2,-height/2,depth/2),
		Vec3(-width/2,height/2,-depth/2),Vec3(width/2,height/2,-depth/2),
		Vec3(width/2,height/2,depth/2),Vec3(-width/2,height/2,depth/2)
	)
	val minimum = Vec3(x,y,z)
	val maximum = Vec3(x+width,y+height,z+depth)

	val equation = (vec: Vec3) => {
		val v = vec - minimum
		val x = ((v.x.toInt%width + width)%width).toInt
		val z = ((v.z.toInt%depth + depth)%depth).toInt
		v.y - heightmap(x)(z)
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
				if(Math.abs(equation(r.equation(pt))) < 5) return pt
				pt = pt + 1/Math.abs(r.direction.z)
			}
		}
		return -1
	}
}