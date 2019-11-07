/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,util},geometry.{Intersectable,Ray,Sphere},util.{Vec3,Timer};
import scala.math.{abs,pow,min,max,random,Pi,floor,tan};
import scala.collection.immutable.List
import annotation.tailrec;

trait Renderable {
	def shape: Intersectable
	def material: Material
	def color: Vec3 => Vec3
}

final case object NilRenderable extends Renderable {
	def shape: Intersectable = null
	def material: Material = null
	def color: Vec3 => Vec3 = null
}

final case class SceneObject(shape: Intersectable, material: Material, color: Vec3=>Vec3) extends Renderable

final case object SceneObject {
	def apply(sh: Intersectable, mat: Material, col: Vec3): SceneObject = {
		SceneObject(sh, mat, (v: Vec3) => {col})
	}
}

trait Light {
	def shape: Intersectable
	def position: Vec3
	def color: Vec3
	def intersectDistance(ray: Ray): Double = {
		shape.intersectDistance(ray)
	}
}

final case class BallLight(r: Double, x: Double, y: Double, z: Double) extends Light {
	def position = Vec3(x, y, z)
	def shape = Sphere(r, position)
	def color = Vec3(1, 1, 1)
}

final case object NilLight extends Light {
	def position = Vec3()
	def shape = null
	def color = Vec3(1, 1, 1)
	override def intersectDistance(ray: Ray): Double = {
		-1.0;
	}
}

final case class Scene(
	head: List[Renderable]=List(),
	lights: List[Light]=List(),
	length: Int=0,
	width: Int=1000,
	height: Int=1000,
	position: Vec3 = Vec3(),
	up: Vec3 = Vec3(0, 1, 0),
	right: Vec3 = Vec3(1, 0, 0),
	fieldOfView: Double=Pi/8,
	spp: Int=1
) {
	val forward = (up^right).normalize
	val cameraPosition = position + right*(width/2) + up*(height/2) - forward*(width/(2*tan(fieldOfView)));
	val toneMap = (x: Double) => {
		Math.max(0, Math.min(1, x))
	}
	def ++(i: Intersectable, m: Material, col: Vec3=>Vec3): Scene = {
		return Scene(SceneObject(i, m, col)::head, lights, length+1, width, height, position, up, right, fieldOfView, spp)
	}
	def ++(i: Intersectable, m: Material, col: Vec3): Scene = {
		return Scene(SceneObject(i, m, col)::head, lights, length+1, width, height, position, up, right, fieldOfView, spp)
	}
	def ++(l: Light): Scene = {
		return Scene(head, l::lights, length+1, width, height, position, up, right, fieldOfView, spp)
	}
	def getPixel(i: Int, j: Int): Vec3 = {
		return position + right*i + up*j
	}
	def getPixelColor(a: Int, b: Int): Vec3 = {
		var sum = Vec3();
		for(i <- 0 until spp) {
			for(j <- 0 until spp) {
				val ray = Ray(cameraPosition, getPixel(a, b) + right*((random + i)/spp) + up*((random + j)/spp))
				val t = trace(ray)
				sum = sum + t
			}
		}
		return sum*(1.0/(spp*spp))
	}
	private def getClosestRenderable(ray: Ray): (Renderable, Vec3) = {
		return head.foldLeft((NilRenderable: Renderable, Vec3()))((prev, rend) => {
			val dist = rend.shape.intersectDistance(ray)
			val distPrev = if(prev._1 != NilRenderable) {
				prev._1.shape.intersectDistance(ray)
			} else {
				-1
			}
			if(dist > 0) {
				if(dist < distPrev || distPrev < 0) {
					(rend, ray.origin + ray.direction*dist)
				} else {
					prev
				}
			} else {
				prev
			}
		})
	}
	private def getClosestLight(ray: Ray): (Light, Vec3) = {
		return lights.foldLeft((NilLight: Light, Vec3()))((prev, light) => {
			val dist = light.shape.intersectDistance(ray)
			val distPrev = if(prev._1 != NilLight) {
				prev._1.shape.intersectDistance(ray)
			} else {
				-1
			}
			if(dist > 0) {
				if(dist < distPrev || distPrev < 0) {
					(light, ray.origin + ray.direction*dist)
				} else {
					prev
				}
			} else {
				prev
			}
		})
		
		// return lights.foldLeft(NilLight: Light)((prev, light) => {
			// val dist = light.intersectDistance(ray)
			// val distPrev = prev.intersectDistance(ray)
			// if(dist > 0) {
				// if(dist < distPrev || distPrev < 0) {
					// light
				// } else {
					// prev
				// }
			// } else {
				// prev
			// }
		// })
	}
	private def getColor(obj: Renderable, intersectPt: Vec3): Vec3 = {
		return obj.color(intersectPt) ** lights.foldLeft(Vec3())((prev, curr) => {
			if(inLineOfSight(intersectPt, curr)) {
				val lightDir = curr.position - intersectPt;
				prev + curr.color * max(0, (obj.shape.getNormal(intersectPt)*lightDir.normalize))
			} else prev;
		})
	}
	private def getLightSize(l: Light): Double = {
		return l match {
			case BallLight(r, x, y, z) => r
			case NilLight => 0.0
		}
	}
	private def inLineOfSight(pt: Vec3, l: Light): Boolean = {
		val lightP = l.position + Vec3(random*2-1, random*2-1, random*2-1)*getLightSize(l)
		val lightDir = (l.position - pt).normalize
		val ray = Ray(pt + lightDir*2, lightP)
		val objHit = getClosestRenderable(ray)
		if(objHit._1 == NilRenderable || ~(lightP - pt) < ~(objHit._2 - pt)) return true
		return false
	}
	def trace(ray: Ray): Vec3 = {
		val closestPoint = getClosestRenderable(ray)
		val objHit = closestPoint._1
		val intersectPt = closestPoint._2
		val hitLight = getClosestLight(ray)
		if(hitLight._1 != NilLight && (objHit == NilRenderable || ~(hitLight._2 - ray.origin) < ~(intersectPt - ray.origin))) return hitLight._1.color
		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
		val col = getColor(objHit, intersectPt)
		if(random > 0.9) return col
		val scatter = Material.scatter(objHit.material, -ray.direction, objHit.shape.getNormal(intersectPt))
		val brdf = Material.brdf(objHit.material, intersectPt, objHit.shape.getNormal(intersectPt))
		val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3)
		val incoming = trace(newRay)
		return (col + incoming*brdf).map(x => toneMap(x))
	}
	def fastTrace(ray: Ray): Vec3 = {
		val closestPoint = getClosestRenderable(ray)
		val objHit = closestPoint._1
		val intersectPt = closestPoint._2
		val hitLight = getClosestLight(ray)
		if(hitLight._1 != NilLight) return hitLight._1.color
		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
		return getColor(objHit, intersectPt)
	}
}