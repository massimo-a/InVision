package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.{abs,min,max,sin,cos};

/** 
 *  @author Massimo Angelillo
 */
final case class SurfaceMarcher(
	equation: Vec3 => Double,
	stepSize: Vec3 => Double,
	boundingBox: Bounded, 
) extends Intersectable {
	def intersectDistance(r: Ray): Double = {
		if(boundingBox.hit(r)) {
			val inter = boundingBox.intersections(r)
			var pt = inter._1
			while(pt < inter._2) {
				val v = r.direction*pt + r.origin
				if(Math.abs(equation(v)) < 2) return pt
				pt = pt + stepSize(v)
			}
		}
		return -1
	}
	
	def gradient(pt: Vec3): Vec3 = {
		val grad_x = (equation(pt)-equation(pt - Vec3(x=0.001)))*1000;
		val grad_y = (equation(pt)-equation(pt - Vec3(y=0.001)))*1000;
		val grad_z = (equation(pt)-equation(pt - Vec3(z=0.001)))*1000;
		return Vec3(grad_x, grad_y, grad_z);
	}
	
	def getNormal(pt: Vec3): Vec3 = {
		return gradient(pt).normalize
	}
	
	def repeat(len: Vec3, c: Double): SurfaceMarcher = {
		val p = (v: Vec3) => {
			equation(Vec3(v.x%len.x - len.x/2.0, v.y%len.y - len.y/2.0, v.z%len.z - len.z/2.0))
		}
		val q = (v: Vec3) => {
			stepSize(Vec3(v.x%len.x - len.x/2.0, v.y%len.y - len.y/2.0, v.z%len.z - len.z/2.0))
		}
		return SurfaceMarcher(p, q, boundingBox.stretch(c, c, c))
	}
	
	def union(surf: SurfaceMarcher): SurfaceMarcher = {
		val func = (v: Vec3) => {
			min(equation(v), surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return SurfaceMarcher(func, v => min(stepSize(v), surf.stepSize(v)), bounds);
	}
	
	def smoothUnion(surf: SurfaceMarcher, k: Double): SurfaceMarcher = {
		val func1 = (v: Vec3) => {
			val h = max(k - abs(equation(v) - surf.equation(v)), 0.0)/k;
			min(equation(v), surf.equation(v)) - h*h*k*0.25;
		}
		val func2 = (v: Vec3) => {
			val h = max(k - abs(stepSize(v) - surf.stepSize(v)), 0.0)/k;
			min(stepSize(v), surf.stepSize(v)) - h*h*k*0.25;
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return SurfaceMarcher(func1, func2, bounds);
	}
	
	def intersect(surf: SurfaceMarcher): SurfaceMarcher = {
		val func = (v: Vec3) => {
			max(equation(v), surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return SurfaceMarcher(func, v => max(stepSize(v), surf.stepSize(v)), bounds);
	}
	
	def smoothIntersect(surf: SurfaceMarcher, k: Double): SurfaceMarcher = {
		val func1 = (v: Vec3) => {
			val h = max(k - abs(equation(v) - surf.equation(v)), 0.0)/k;
			max(equation(v), surf.equation(v)) - h*h*k*0.25;
		}
		val func2 = (v: Vec3) => {
			val h = max(k - abs(stepSize(v) - surf.stepSize(v)), 0.0)/k;
			max(stepSize(v), surf.stepSize(v)) - h*h*k*0.25;
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return SurfaceMarcher(func1, func2, bounds);
	}
	
	def subtract(surf: SurfaceMarcher): SurfaceMarcher = {
		val func = (v: Vec3) => {
			max(equation(v), -surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return SurfaceMarcher(func, v => max(stepSize(v), -surf.stepSize(v)), bounds);
	}
	
	def translate(x: Double, y: Double, z: Double): SurfaceMarcher = {
		return SurfaceMarcher(v => (equation(v - Vec3(x, y, z))), v => stepSize(v-Vec3(x, y, z)), boundingBox.translate(x, y, z))
	}
	
	def translate(u: Vec3): SurfaceMarcher = {
		return SurfaceMarcher(v => equation(v - u), v => stepSize(v-u), boundingBox.translate(u.x, u.y, u.z))
	}
	
	def distort(f: Vec3 => Double, p: Double): SurfaceMarcher = {
		return SurfaceMarcher(v => equation(v) + f(v), v => (stepSize(v) + f(v))/p, boundingBox)
	}
	
	def rotateX(rad: Double): SurfaceMarcher = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(v.x, c*v.y - s*v.z, s*v.y + c*v.z)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return SurfaceMarcher(func, v => stepSize(rotatePoint(v)), boundingBox.rotateWith(rotatePoint));
	}
	
	def rotateY(rad: Double): SurfaceMarcher = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(c*v.x + s*v.z, v.y, c*v.z - s*v.x)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return SurfaceMarcher(func, v => stepSize(rotatePoint(v)), boundingBox.rotateWith(rotatePoint));
	}
	
	def rotateZ(rad: Double): SurfaceMarcher = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(c*v.x - s*v.y, c*v.y + s*v.x, v.z)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return SurfaceMarcher(func, v => stepSize(rotatePoint(v)), boundingBox.rotateWith(rotatePoint));
	}
	
	def rotate(rad: Vec3): SurfaceMarcher = {
		return this.rotateX(rad.x).rotateY(rad.y).rotateZ(rad.z);
	}
	
	def rotate(x: Double, y: Double, z: Double): SurfaceMarcher = {
		return this.rotateX(x).rotateY(y).rotateZ(z);
	}
}

object SurfaceMarcher {
	def Sphere(r: Double): SurfaceMarcher = {
		return SurfaceMarcher(
			(v: Vec3) => {v.magnitude - r},
			(v: Vec3) => {v.magnitude - r},
			BoundingBox(Vec3(-r-5, -r-5, -r-5), Vec3(2*r+10, 0, 0), Vec3(0, 2*r+10, 0), Vec3(0, 0, 2*r+10))
		)
	}
	
	def Box(width: Double, height: Double, depth: Double): SurfaceMarcher = {
		return SurfaceMarcher(
			(v: Vec3) => {
				val d = (v).map(x => abs(x)) - Vec3(width/2,height/2,depth/2);
				val a = d.map(x => max(x, 0.0)).magnitude;
				a + min(max(d.x, max(d.y, d.z)), 0)
			},
			(v: Vec3) => {
				val d = (v).map(x => abs(x)) - Vec3(width/2,height/2,depth/2);
				val a = d.map(x => max(x, 0.0)).magnitude;
				a + min(max(d.x, max(d.y, d.z)), 0)
			},
			BoundingBox(Vec3(-width/2,-height/2,-depth/2), Vec3(width, 0, 0), Vec3(0, height, 0), Vec3(0, 0, depth))
		)
	}
	
	def Box(b: Vec3): SurfaceMarcher = {
		return Box(b.x,b.y,b.z)
	}
	
	def Cylinder(r: Double, h: Double): SurfaceMarcher = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r, abs(p.y) - h/2);
			min(max(d.x,d.y),0.0) + d.map(x => max(x, 0)).magnitude
		}
		return SurfaceMarcher(
			func,
			func,
			BoundingBox(Vec3(-r-5,-h/2-5,-r-5), Vec3(2*r+10, 0, 0), Vec3(0, h+10, 0), Vec3(0, 0, 2*r+10))
		)
	}
	
	def Torus(r1: Double, r2: Double): SurfaceMarcher = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r1, p.y);
			d.magnitude - r2;
		}
		return SurfaceMarcher(
			func,
			func,
			BoundingBox(Vec3(-r1-2*r2,-r2,-r1-2*r2), Vec3(2*(r1+2*r2), 0, 0), Vec3(0, r2*2, 0), Vec3(0, 0, 2*(r1+2*r2)))
		)
	}
}