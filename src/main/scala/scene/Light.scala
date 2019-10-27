/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,util},geometry.{Intersectable,Sphere,Ray},util.Vec3;
import scala.math.{random,sqrt};

trait Light {
	def shape: Intersectable
	def position: Vec3
	def emission: Vec3
	def intersectDistance(ray: Ray): Double = {
		shape.intersectDistance(ray)
	}
}

case class BallLight(r: Double, x: Double, y: Double, z: Double) extends Light {
	def position = Vec3(x, y, z)
	def shape = Sphere(r, position)
	def emission = Vec3(1, 1, 1)
}

case object NilLight extends Light {
	def position = Vec3()
	def shape = null
	def emission = Vec3(1, 1, 1)
	override def intersectDistance(ray: Ray): Double = {
		-1.0;
	}
}