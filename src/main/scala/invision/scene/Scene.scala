package invision.scene
import invision.geometry.Ray
import invision.scene.renderable.{NilLight, NilRenderable}
import invision.util.Vec3

import scala.math.random

final case class Camera(position: Vec3, fieldOfView: Double)

trait Renderer {
	val world: World
	val camera: Camera
	val spp: Int
	val width: Int
	val height: Int

	def getPixelColor(a: Int, b: Int): Vec3 = {
		var sum = Vec3()
		for(i <- 0 until spp) {
			for(j <- 0 until spp) {
				val ray = Ray(camera.position, Vec3(a + (random + i)/spp, b + (random + j)/spp))
				val t = trace(ray)
				sum = sum + t
			}
		}
		sum*(1.0/(spp*spp))
	}

	def render(): Array[Array[Int]] = {
		Array.ofDim[Int](width, height)
			.map(_.map(_ => -1)).zipWithIndex
			.map(a => {
				new Array[Int](width).zipWithIndex.map(b => {
					val t = getPixelColor(a._2, height - b._2)
					val rgb = (((t.x*255).toInt & 0x0ff) << 16) | (((t.y*255).toInt & 0x0ff) << 8) | ((t.z*255).toInt & 0x0ff)
					rgb
				})
			})
	}

	def trace(r: Ray): Vec3
}

final case class RayTracer(world: World, camera: Camera, spp: Int, width: Int, height: Int) extends Renderer {
	val toneMap: Double => Double = (x: Double) => {
		x/(x+1)
	}

	def trace(r: Ray): Vec3 = {
		val closestPoint = world.getClosestRenderable(r)
		val objHit = closestPoint._1
		val intersectPt = closestPoint._2
		val hitLight = world.getClosestLight(r)
		if(hitLight._1 != NilLight && (objHit == NilRenderable || ~(hitLight._2 - r.origin) < ~(intersectPt - r.origin))) return hitLight._1.color
		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
		val col = world.getColor(objHit, intersectPt, r)
		val scatter = objHit.material.scatter(-r.direction, objHit.shape.getNormal(intersectPt))
		val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3)
		val brdf = objHit.material.brdf(-r.direction, newRay.direction, objHit.shape.getNormal(intersectPt))
		if(random > brdf*0.9) return col
		val lightDir = (world.lights.head.position - intersectPt).normalize()
		val lightRay = Ray(intersectPt + lightDir, intersectPt + lightDir*3)
		val incoming = trace(newRay)
		(col + incoming*brdf).map(x => toneMap(x))
	}

//		def trace(ray: Ray): Vec3 = {
//			val closestPoint = world.getClosestRenderable(ray)
//			val objHit = closestPoint._1
//			val intersectPt = closestPoint._2
//			val hitLight = world.getClosestLight(ray)
//			if(hitLight._1 != NilLight) return hitLight._1.color
//			if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
//			world.getColor(objHit, intersectPt, ray)
//		}
}

//final case class Scene(
//	head: List[Renderable]=List(),
//	lights: List[Light]=List(),
//	length: Int=0,
//	width: Int=1000,
//	height: Int=1000,
//	position: Vec3 = Vec3(),
//	up: Vec3 = Vec3(0, 1, 0),
//	right: Vec3 = Vec3(1, 0, 0),
//	fieldOfView: Double=Pi/8,
//	spp: Int=1
//) {
//	val forward: Vec3 = (up ^ right).normalize()
//	val cameraPosition: Vec3 = position + right*(width/2) + up*(height/2) - forward*(width/(2*tan(fieldOfView)))
//
//	@scala.annotation.tailrec
//	def sample(shape: Intersectable, ray: Ray): Ray = {
//		val tmax = shape.intersectDistance(ray)
//		val s = -0.01*Math.log(random)
//		if(s > tmax) {
//			Ray(ray.origin + ray.direction*(tmax+1), ray.origin + ray.direction*(tmax+2))
//		}
//		else {
//			val theta = random() * Pi
//			val phi = random() * 2 * Pi
//			val newDir = Vec3(Math.sin(theta) * Math.cos(phi), Math.sin(theta) * Math.sin(phi), Math.cos(theta))
//			val newRay = Ray(ray.origin + ray.direction * s, ray.origin + ray.direction * s + newDir)
//			sample(shape, newRay)
//		}
//	}
//
//	def testTrace(ray: Ray): Vec3 = {
//		val closestPoint = getClosestRenderable(ray)
//		val objHit = closestPoint._1
//		val intersectPt = closestPoint._2
//		val hitLight = getClosestLight(ray)
//		if(hitLight._1 != NilLight && (objHit == NilRenderable || ~(hitLight._2 - ray.origin) < ~(intersectPt - ray.origin))) return hitLight._1.color
//		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
//		val col = getColor(objHit, intersectPt, ray)
//		val newRay = sample(objHit.shape, Ray(intersectPt + ray.direction*2, intersectPt + ray.direction*4))
//		val brdf = objHit.material.brdf(-ray.direction, newRay.direction, objHit.shape.getNormal(newRay.origin))
//		if(random > brdf*0.9) return col
//		val incoming = trace(newRay)
//		(col + incoming*brdf).map(x => toneMap(x))
//	}
//	def fastTrace(ray: Ray): Vec3 = {
//		val closestPoint = getClosestRenderable(ray)
//		val objHit = closestPoint._1
//		val intersectPt = closestPoint._2
//		val hitLight = getClosestLight(ray)
//		if(hitLight._1 != NilLight) return hitLight._1.color
//		if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
//		getColor(objHit, intersectPt, ray)
//	}
//}