package raytracing.scene
import raytracing.geometry.{Intersectable, Ray, Sphere}
import raytracing.util.Vec3

import scala.collection.immutable.List
import scala.collection.parallel.mutable.ParArray
import scala.math.{Pi, max, random, tan}

/** 
 *  @author Massimo Angelillo
 */
trait Renderable {
	def shape: Intersectable
	def material: Material
	def color: Vec3 => Vec3
}

case object NilRenderable extends Renderable {
	def shape: Intersectable = null
	def material: Material = null
	def color: Vec3 => Vec3 = null
}

final case class SceneObject(shape: Intersectable, material: Material, color: Vec3=>Vec3) extends Renderable

case object SceneObject {
	def apply(sh: Intersectable, mat: Material, col: Vec3): SceneObject = {
		SceneObject(sh, mat, (_: Vec3) => col)
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
	def position: Vec3 = Vec3(x, y, z)
	def shape: Intersectable = Sphere(r, position)
	def color: Vec3 = Vec3(1, 1, 1)
}

case object NilLight extends Light {
	def position: Vec3 = Vec3()
	def shape: Intersectable = null
	def color: Vec3 = Vec3(1, 1, 1)
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
	val forward: Vec3 = (up ^ right).normalize()
	val cameraPosition: Vec3 = position + right*(width/2) + up*(height/2) - forward*(width/(2*tan(fieldOfView)));
	val toneMap: Double => Double = (x: Double) => {
		x/(x+1)
	}
	def ++(i: Intersectable, m: Material, col: Vec3=>Vec3): Scene = {
		Scene(SceneObject(i, m, col)::head, lights, length+1, width, height, position, up, right, fieldOfView, spp)
	}
	def ++(i: Intersectable, m: Material, col: Vec3): Scene = {
		Scene(SceneObject(i, m, col)::head, lights, length+1, width, height, position, up, right, fieldOfView, spp)
	}
	def ++(l: Light): Scene = {
		Scene(head, l::lights, length+1, width, height, position, up, right, fieldOfView, spp)
	}
	def getPixel(i: Int, j: Int): Vec3 = {
		position + right*i + up*j
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
		sum*(1.0/(spp*spp))
	}
	private def getClosestRenderable(ray: Ray): (Renderable, Vec3) = {
		head.foldLeft((NilRenderable: Renderable, Vec3()))((prev, rend) => {
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
		lights.foldLeft((NilLight: Light, Vec3()))((prev, light) => {
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
	}
	private def getColor(obj: Renderable, intersectPt: Vec3, ray: Ray): Vec3 = {
		obj.color(intersectPt) ** lights.foldLeft(Vec3())((prev, curr) => {
			if(inLineOfSight(intersectPt, curr)) {
				val lightDir = curr.position - intersectPt;
				prev + curr.color * max(0, (obj.shape.getNormal(intersectPt)*lightDir.normalize))
			} else prev;
		})
	}
	private def getLightSize(l: Light): Double = {
		l match {
			case BallLight(r, x, y, z) => r
			case NilLight => 0.0
		}
	}
	private def inLineOfSight(pt: Vec3, l: Light): Boolean = {
		val lightP = l.position + Vec3(random*2-1, random*2-1, random*2-1)*getLightSize(l)
		val lightDir = (l.position - pt).normalize()
		val ray = Ray(pt + lightDir*2, lightP)
		val objHit = getClosestRenderable(ray)
		if(objHit._1 == NilRenderable || ~(lightP - pt) < ~(objHit._2 - pt)) return true
		false
	}
	def trace(ray: Ray): Vec3 = {
		val closestPoint = getClosestRenderable(ray)
		val objHit = closestPoint._1
		val intersectPt = closestPoint._2
		val hitLight = getClosestLight(ray)
		if(hitLight._1 != NilLight && (objHit == NilRenderable || ~(hitLight._2 - ray.origin) < ~(intersectPt - ray.origin))) return hitLight._1.color
		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
		val col = getColor(objHit, intersectPt, ray)
		val scatter = objHit.material.scatter(-ray.direction, objHit.shape.getNormal(intersectPt))
		val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3)
		val brdf = objHit.material.brdf(-ray.direction, newRay.direction, objHit.shape.getNormal(intersectPt))
		if(random > brdf*0.9) return col
		val lightDir = (lights.head.position - intersectPt).normalize()
		val lightRay = Ray(intersectPt + lightDir, intersectPt + lightDir*3)
		val incoming = trace(newRay)
		(col + incoming*brdf).map(x => toneMap(x))
	}

	@scala.annotation.tailrec
	def sample(shape: Intersectable, ray: Ray): Ray = {
		val tmax = shape.intersectDistance(ray)
		val s = -0.01*Math.log(random)
		if(s > tmax) {
			Ray(ray.origin + ray.direction*(tmax+1), ray.origin + ray.direction*(tmax+2))
		}
		else {
			val theta = random() * Pi
			val phi = random() * 2 * Pi
			val newDir = Vec3(Math.sin(theta) * Math.cos(phi), Math.sin(theta) * Math.sin(phi), Math.cos(theta))
			val newRay = Ray(ray.origin + ray.direction * s, ray.origin + ray.direction * s + newDir)
			sample(shape, newRay)
		}
	}

	def testTrace(ray: Ray): Vec3 = {
		val closestPoint = getClosestRenderable(ray)
		val objHit = closestPoint._1
		val intersectPt = closestPoint._2
		val hitLight = getClosestLight(ray)
		if(hitLight._1 != NilLight && (objHit == NilRenderable || ~(hitLight._2 - ray.origin) < ~(intersectPt - ray.origin))) return hitLight._1.color
		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
		val col = getColor(objHit, intersectPt, ray)
		val newRay = sample(objHit.shape, Ray(intersectPt + ray.direction*2, intersectPt + ray.direction*4))
		val brdf = objHit.material.brdf(-ray.direction, newRay.direction, objHit.shape.getNormal(newRay.origin))
		if(random > brdf*0.9) return col
		val incoming = trace(newRay)
		(col + incoming*brdf).map(x => toneMap(x))
	}
	def fastTrace(ray: Ray): Vec3 = {
		val closestPoint = getClosestRenderable(ray)
		val objHit = closestPoint._1
		val intersectPt = closestPoint._2
		val hitLight = getClosestLight(ray)
		if(hitLight._1 != NilLight) return hitLight._1.color
		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
		getColor(objHit, intersectPt, ray)
	}
	def render(): ParArray[ParArray[Int]] = {
		Array.ofDim[Int](width, height).par
			.map(_.par.map(x => -1)).zipWithIndex
			.map(a => {
				new Array[Int](width).par.zipWithIndex.map(b => {
				val t = getPixelColor(a._2, height - b._2)
				val rgb = (((t.x*255).toInt & 0x0ff) << 16) | (((t.y*255).toInt & 0x0ff) << 8) | ((t.z*255).toInt & 0x0ff)
					rgb
				})
			})
	}
}