/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,util},geometry.{Intersectable,BoundedSDF,Ray},util.Vec3;
import scala.math.{random,sqrt};

object NoLights extends Lighting {
	override def isEmpty(): Boolean = {return true}
}
case class Lighting(
	next: Lighting=NoLights,
	redEmission: Double = 1.0,
	greenEmission: Double = 1.0,
	blueEmission: Double = 1.0,
	x: Double = 0.0,
	y: Double = 0.0,
	z: Double = 0.0,
	size: Double = 10,
	falloff: (Double => Double) = _ => 1.0,
	visibility: Boolean = true
) {
	val emission = Vec3(redEmission, greenEmission, blueEmission);
	val position = Vec3(x, y, z);
	val shape = BoundedSDF.Sphere(size).translate(x,y,z);
	private def copy(n: Lighting): Lighting = {
		return Lighting(n, redEmission, greenEmission, blueEmission, x, y, z, size, falloff);
	}
	def isEmpty(): Boolean = {return false}
	def ++(l: Lighting): Lighting = {
		return l.copy(this)
	}
	private def fold(func: Lighting => Vec3, accu: Vec3): Vec3 = {
		if(isEmpty) return accu
		return next.fold(func, accu + func(this));
	}
	def fold(func: Lighting => Vec3): Vec3 = {
		return fold(func, Vec3());
	}
	def searchForIntersection(ray: Ray): Lighting = {
		if(shape.intersectDistance(ray) > 0) return this;
		if(!next.isEmpty) return next.searchForIntersection(ray);
		return NoLights;
	}
}