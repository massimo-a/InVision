package raytracing.scene;
import raytracing.{geometry,util},geometry.{Surface,Ray},util.{Vec3,Timer};
import scala.math.{abs,pow,min,max,random,Pi,floor,tan};
import annotation.tailrec;

case class SceneObject(shape: Surface, material: Shader, next: SceneObject) {
	def color = (v: Vec3) => {
		return material.color
	}
}

case class Scene(
	head: SceneObject=null,
	lights: Lighting=NoLights,
	length: Int=0,
	width: Int=1000,
	height: Int=1000,
	x: Int=0, y: Int=0, z: Int=0,
	fieldOfView: Double=Pi/8,
	spp: Int=1
) {
	val position = Vec3(x, y, z); //position of lower left corner of screen
	val cameraPosition = Vec3(x + width/2, y + height/2, z - width/(2*tan(fieldOfView)))
	
	private def lerp(y0: Double, y1: Double, t: Double): Double = {
		return y0*(1-t) + y1*t;
	}
	private def colorLerp(a: Vec3, b: Vec3, c: Double): Vec3 = {
		return Vec3(lerp(a.x,b.x,c),lerp(a.y,b.y,c),lerp(a.z,b.z,c))
	}
	def ++(i: Surface, m: Shader): Scene = {
		return Scene(SceneObject(i, m, head), lights, length+1, width, height, x, y, z, fieldOfView, spp);
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
	@tailrec private def getClosestObject(ray: Ray, curr: SceneObject, closeObj: SceneObject, closeD: Double): (SceneObject, Vec3) = {
		if(curr == null) return (closeObj, ray.origin + ray.direction*closeD)
		var closestObj: SceneObject = closeObj;
		var closestDist: Double = closeD;
		val dist = curr.shape.intersectDistance(ray);
		if(dist > 0) {
			if(closestDist == -1 || closestDist > dist) {
				closestObj = curr;
				closestDist = dist;
			}
		}
		return getClosestObject(ray, curr.next, closestObj, closestDist)
	}
	private def getClosestObject(ray: Ray): (SceneObject, Vec3) = {
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
				return colorLerp(Vec3(0,0,0), Vec3(0.35,0.35,1), (ray.direction*Vec3(0,1,0)+0.38268)/1.38268)
			} else {
				return Vec3()
			}
		}
		val col = objHit.color ** lights.fold(x => {
			val lightDir = x.position - intersectPt;
			if(inLineOfSight(intersectPt, x)) {
				x.emission * max(0, objHit.shape.getAngleWithNormal(intersectPt, lightDir.normalize)*x.falloff(lightDir.magnitude));
			} else Vec3();
		})
		if(random > 0.9) return col;
		val scatter = objHit.material.scatterLight(-ray.direction, objHit.shape.getNormal(intersectPt));
		val brdf = objHit.material.brdf(-ray.direction, scatter, objHit.shape.getNormal(intersectPt))
		val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3);
		val incoming = trace(newRay);
		return (col + incoming*brdf).map(x => x/(x+2));
	}
}