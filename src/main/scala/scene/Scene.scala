/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,util},geometry.{Intersectable,Ray},util.{Vec3,Timer};
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

final case class Scene(
	head: List[Renderable]=List(),
	lights: List[Light]=List(),
	length: Int=0,
	width: Int=1000,
	height: Int=1000,
	x: Int=0, y: Int=0, z: Int=0,
	fieldOfView: Double=Pi/8,
	spp: Int=1
) {
	val position = Vec3(x, y, z); //position of lower left corner of screen
	val cameraPosition = Vec3(x + width/2, y + height/2, z - width/(2*tan(fieldOfView)));
	val toneMap = (x: Double) => {
		x/(x+0.9)
	}
	
	def ++(i: Intersectable, m: Material, col: Vec3=>Vec3): Scene = {
		return Scene(SceneObject(i, m, col)::head, lights, length+1, width, height, x, y, z, fieldOfView, spp)
	}
	def ++(i: Intersectable, m: Material, col: Vec3): Scene = {
		return Scene(SceneObject(i, m, col)::head, lights, length+1, width, height, x, y, z, fieldOfView, spp)
	}
	def ++(l: Light): Scene = {
		return Scene(head, l::lights, length+1, width, height, x, y, z, fieldOfView, spp)
	}
	def getPixel(i: Int, j: Int): Vec3 = {
		return position + Vec3(i, j, 0);
	}
	def getPixelColor(a: Int, b: Int): Vec3 = {
		var sum = Vec3();
		for(i <- 0 until spp) {
			for(j <- 0 until spp) {
				val ray = Ray(cameraPosition, getPixel(a, b) + Vec3((random + i)/spp, (random + j)/spp))
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
	private def getClosestLight(ray: Ray): Light = {
		return lights.foldLeft(NilLight: Light)((prev, light) => {
			val dist = light.intersectDistance(ray)
			val distPrev = prev.intersectDistance(ray)
			if(dist > 0) {
				if(dist < distPrev || distPrev < 0) {
					light
				} else {
					prev
				}
			} else {
				prev
			}
		})
	}
	private def getColor(obj: Renderable, intersectPt: Vec3): Vec3 = {
		return obj.color(intersectPt) ** lights.foldLeft(Vec3())((prev, curr) => {
			if(inLineOfSight(intersectPt, curr)) {
				val lightDir = curr.position - intersectPt;
				prev + curr.emission * max(0, (obj.shape.getNormal(intersectPt)*lightDir.normalize))
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
		if(hitLight != NilLight) return hitLight.emission
		if(objHit == NilRenderable) {
			if(~(ray.origin - cameraPosition) < 1) {
				return Vec3(0,0,0).lerp(Vec3(0.8,0.8,1.0), (ray.direction*Vec3(0,1,0)+0.38268)/1.38268) //should be fixed
			} else {
				return Vec3()
			}
		}
		val col = getColor(objHit, intersectPt)
		if(random > 0.9) return col
		val scatter = Material.scatter(objHit.material, -ray.direction, objHit.shape.getNormal(intersectPt))
		val brdf = Material.brdf(objHit.material, intersectPt, objHit.shape.getNormal(intersectPt))
		val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3)
		val incoming = trace(newRay)
		return (col + incoming*brdf).map(x => toneMap(x))
	}
}