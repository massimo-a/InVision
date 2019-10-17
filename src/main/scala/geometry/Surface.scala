/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.abs;

trait SurfaceMarcher extends Intersectable {
	val equation: Vec3 => Double;
	def intersectDistance(r: Ray): Double;
	def gradient(pt: Vec3): Vec3 = {
		val grad_x = (equation(pt)-equation(pt - Vec3(x=0.001)))*1000;
		val grad_y = (equation(pt)-equation(pt - Vec3(y=0.001)))*1000;
		val grad_z = (equation(pt)-equation(pt - Vec3(z=0.001)))*1000;
		return Vec3(grad_x, grad_y, grad_z);
	}
	def getNormal(pt: Vec3): Vec3 = {
		return gradient(pt).normalize
	}
}

final case class ImpSurf(
	equation: Vec3 => Double,
	position: Vec3,
	boundingBox: Bounded
) extends SurfaceMarcher {
	def intersectDistance(r: Ray): Double = {
		if(boundingBox.hit(r)) {
			val inter = boundingBox.intersections(r)
			var pt = inter._1
			while(pt < inter._2) {
				val v = r.equation(pt)
				if(Math.abs(equation(v)) < 2) return pt
				pt = pt + Math.abs(equation(v))/gradient(v).magnitude
			}
		}
		return -1
	}
}

final case class Terrain(
	heightmap: Array[Array[Double]],
	height: Double,
	x:Double=0,y:Double=0,z:Double=0
) extends SurfaceMarcher {
	val width = heightmap.length;
	val depth = heightmap(0).length
	val boundingBox = BoundingBox(Vec3(x, y, z), Vec3(width, 0, 0), Vec3(0, height, 0), Vec3(0, 0, depth))

	val equation = (vec: Vec3) => {
		val v = vec - Vec3(x, y, z)
		val i = ((v.x.toInt%width + width)%width).toInt
		val j = ((v.z.toInt%depth + depth)%depth).toInt
		v.y - heightmap(i)(j)*height
	}
	override def gradient(pt: Vec3): Vec3 = {
		val grad_x = (equation(pt)-equation(pt - Vec3(x=1)));
		val grad_y = (equation(pt)-equation(pt - Vec3(y=1)));
		val grad_z = (equation(pt)-equation(pt - Vec3(z=1)));
		return Vec3(grad_x, grad_y, grad_z);
	}
	def intersectDistance(r: Ray): Double = {
		if(boundingBox.hit(r)) {
			val inter = boundingBox.intersections(r)
			var pt = inter._1
			while(pt < inter._2) {
				val v = r.equation(pt)
				if(equation(v) < 2) return pt
				pt = pt + 2
			}
		}
		return -1
	}
}