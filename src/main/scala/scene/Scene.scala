/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,util},geometry.{Intersectable,Ray},util.{Vec3,Timer};
import scala.math.{abs,pow,min,max,random,Pi,floor,tan};
import annotation.tailrec;

case class Renderable(shape: Intersectable, material: Material, color: Vec3=>Vec3, next: Renderable)

case class Scene(
	head: Renderable=null,
	lights: Lighting=NoLights,
	length: Int=0,
	width: Int=1000,
	height: Int=1000,
	x: Int=0, y: Int=0, z: Int=0,
	fieldOfView: Double=Pi/8,
	spp: Int=1
) {
	val position = Vec3(x, y, z); //position of lower left corner of screen
	val cameraPosition = Vec3(x + width/2, y + height/2, z - width/(2*tan(fieldOfView)));
	
	def ++(i: Intersectable, m: Material, col: Vec3=>Vec3): Scene = {
		return Scene(Renderable(i, m, col, head), lights, length+1, width, height, x, y, z, fieldOfView, spp);
	}
	def ++(l: Lighting): Scene = {
		return Scene(head, lights++l, length+1, width, height, x, y, z, fieldOfView, spp);
	}
	def getPixel(i: Int, j: Int): Vec3 = {
		return position + Vec3(i, j, 0);
	}
	def getPixelColor(a: Int, b: Int): Vec3 = {
		var sum = Vec3();
		for(i <- 0 until spp) {
			for(j <- 0 until spp) {
				val ray = Ray(cameraPosition, getPixel(a, b) + Vec3((random + i)/spp, (random + j)/spp));
				val t = trace(ray);
				sum = sum + t;
			}
		}
		return sum*(1.0/(spp*spp));
	}
	@tailrec private def getClosestObject(ray: Ray, curr: Renderable, closeObj: Renderable, closeD: Double): (Renderable, Vec3) = {
		if(curr == null) return (closeObj, ray.origin + ray.direction*closeD)
		val dist = curr.shape.intersectDistance(ray);
		val (closestObj, closestDist) = if(dist > 0 && (closeD == -1 || closeD > dist)) {
			(curr, dist)
		} else {
			(closeObj, closeD)
		}
		return getClosestObject(ray, curr.next, closestObj, closestDist)
	}
	private def getClosestObject(ray: Ray): (Renderable, Vec3) = {
		return getClosestObject(ray, head, null, -1);
	}
	private def inLineOfSight(pt: Vec3, l: Lighting): Boolean = {
		val lightP = l.position + Vec3(random*2-1, random*2-1, random*2-1)*l.size;
		val lightDir = (l.position - pt).normalize
		val ray = Ray(pt + lightDir*2, lightP);
		val objHit = getClosestObject(ray);
		if(objHit._1 == null || ~(lightP - pt) < ~(objHit._2 - pt)) return true
		return false;
	}
	def trace(ray: Ray): Vec3 = {
		val closestPoint = getClosestObject(ray);
		val objHit = closestPoint._1;
		val intersectPt = closestPoint._2;
		val hitLight = lights.searchForIntersection(ray);
		if(hitLight != NoLights && hitLight.visibility) return hitLight.emission;
		if(objHit == null) {
			if(~(ray.origin - cameraPosition) < 1) {
				return Vec3(0,0,0).lerp(Vec3(0.8,0.8,1.0), (ray.direction*Vec3(0,1,0)+0.38268)/1.38268) //should be fixed
			} else {
				return Vec3()
			}
		}
		val col = objHit.color(intersectPt) ** lights.fold(x => {
			val lightDir = x.position - intersectPt;
			if(inLineOfSight(intersectPt, x)) {
				x.emission * max(0, (objHit.shape.getNormal(intersectPt)*lightDir.normalize)*x.falloff(lightDir.magnitude));
			} else Vec3();
		})
		if(random > 0.9) return col;
		val scatter = Material.scatter(objHit.material, -ray.direction, objHit.shape.getNormal(intersectPt));
		val brdf = Material.brdf(objHit.material, intersectPt, objHit.shape.getNormal(intersectPt));
		val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3);
		val incoming = trace(newRay);
		return (col + incoming*brdf).map(x => {
			x/(x+0.75);
		})
	}
}