package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.abs;

/** description
 *  
 *  @author Massimo Angelillo
 *  @constructor 
 *  @param heightmap
 *  @param height
 *  @param position
 */
final case class Terrain(
	heightmap: Array[Array[Double]],
	height: Double,
	position: Vec3
) extends Intersectable {
	val width = heightmap.length;
	val depth = heightmap(0).length
	val boundingBox = BoundingBox(position, Vec3(width, 0, 0), Vec3(0, height, 0), Vec3(0, 0, depth))

	val equation = (vec: Vec3) => {
		val v = vec - position
		val i = ((v.x.toInt%width + width)%width).toInt
		val j = ((v.z.toInt%depth + depth)%depth).toInt
		v.y - heightmap(i)(j)*height
	}
	
	def gradient(pt: Vec3): Vec3 = {
		val grad_x = (equation(pt)-equation(pt - Vec3(x=1)));
		val grad_y = (equation(pt)-equation(pt - Vec3(y=1)));
		val grad_z = (equation(pt)-equation(pt - Vec3(z=1)));
		return Vec3(grad_x, grad_y, grad_z);
	}
	
	def getNormal(pt: Vec3): Vec3 = {
		return gradient(pt).normalize;
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