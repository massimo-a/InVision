/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.{Vec3,Noise};
import scala.math.{abs,max,min,cos,sin};

/* 
** An SDF takes a point in space (given as a vector) and returns
** the distance to the closest point on the SDF. It is 'signed'
** because it returns positive distance when the point is outside the
** SDF and negative when inside the SDF.
*/
case class BoundedSDF(
	equation: Vec3 => Double,
	boundingBox: Bounds
) extends SurfaceMarcher {
	private def gradient(f: Vec3 => Double, pt: Vec3): Vec3 = {
		val grad_x = (f(pt)-f(pt - Vec3(x=0.01)))*100;
		val grad_y = (f(pt)-f(pt - Vec3(y=0.01)))*100;
		val grad_z = (f(pt)-f(pt - Vec3(z=0.01)))*100;
		return Vec3(grad_x, grad_y, grad_z);
	}
	
	/*
	** union merges two BoundedSDFs together. Visually there is no
	** difference between keeping the BoundedSDFs separate or unioning
	** them, but by unioning the BoundedSDFs the program treats the two
	** BoundedSDFs as one, and therefore runs faster --
	** (so long as they are relatively close to each other).
	*/
	def union(surf: BoundedSDF): BoundedSDF = {
		val func = (v: Vec3) => {
			min(equation(v), surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSDF(func, bounds);
	}
	
	/*
	** The same as a normal union, but the point of contact between
	** the two BoundedSDFs has a smooth transition, determined by k.
	** The higher the value of k, the 'smoother' the transition
	*/
	def smoothUnion(surf: BoundedSDF, k: Double): BoundedSDF = {
		val func = (v: Vec3) => {
			val h = max(k - abs(equation(v) - surf.equation(v)), 0.0)/k;
			min(equation(v), surf.equation(v)) - h*h*k*0.25;
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSDF(func, bounds);
	}
	
	// Takes two BoundedSDFs and returns the BoundedSDF that is contained by both
	def intersect(surf: BoundedSDF): BoundedSDF = {
		val func = (v: Vec3) => {
			max(equation(v), surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSDF(func, bounds);
	}
	/*
	** Takes two BoundedSDFs and returns the BoundedSDF
	** that is contained by the first, but not the second
	** NOTE: This function is not commutative,
	** i.e a.subtract(b) =/= b.subtract(a)
	*/
	def subtract(surf: BoundedSDF): BoundedSDF = {
		val func = (v: Vec3) => {
			max(equation(v), -surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSDF(func, bounds);
	}
	
	/*
	** These methods allow translating and rotating
	** a BoundedSDF in 3D space. The rotating methods take
	** the angle as a radian, not a degree.
	** When the shape is rotated, it is rotated about the axis
	** according to the right-hand rule
	*/
	def translate(x: Double, y: Double, z: Double): BoundedSDF = {
		return BoundedSDF(v => (equation(v - Vec3(x, y, z))), boundingBox.translate(x, y, z))
	}
	
	/*
	** The rotate functions should always be called BEFORE
	** the translate function, for it to work the way you think it will
	*/
	def rotateX(rad: Double): BoundedSDF = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(v.x, c*v.y - s*v.z, s*v.y + c*v.z)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return BoundedSDF(func, boundingBox.rotateWith(rotatePoint));
	}
	def rotateY(rad: Double): BoundedSDF = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(c*v.x + s*v.z, v.y, c*v.z - s*v.x)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return BoundedSDF(func, boundingBox.rotateWith(rotatePoint));
	}
	def rotateZ(rad: Double): BoundedSDF = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(c*v.x - s*v.y, c*v.y + s*v.x, v.z)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return BoundedSDF(func, boundingBox.rotateWith(rotatePoint));
	}
	def rotate(rad: Vec3): BoundedSDF = {
		return this.rotateX(rad.x).rotateY(rad.y).rotateZ(rad.z);
	}
	def rotate(x: Double, y: Double, z: Double): BoundedSDF = {
		return this.rotateX(x).rotateY(y).rotateZ(z);
	}
	
	private def findRoot(func: Double => Double, pt: Double, dist: Double): Double = {
		val f = abs(func(pt))
		if(f < 5) return pt;
		if(dist < 0) return -1;
		return findRoot(func, pt + f, dist - f);
	}
	def intersectDistance(r: Ray): Double = {
		val pts = boundingBox.intersections(r);
		if(pts._1 < pts._2) {
			return findRoot((x: Double) => {equation(r.equation(x))}, pts._1, (pts._2 - pts._1));
		} else return -1;
	}
}

object BoundedSDF {
	def Sphere(r: Double): BoundedSDF = {
		return BoundedSDF(
			(v: Vec3) => {v.magnitude - r},
			Bounds(Vec3(-r-2, -r-2, -r-2), Vec3(2*r+4, 0, 0), Vec3(0, 2*r+4, 0), Vec3(0, 0, 2*r+4))
		)
	}
	
	def Box(width: Double, height: Double, depth: Double): BoundedSDF = {
		return BoundedSDF(
			(v: Vec3) => {
				val d = (v).map(x => abs(x)) - Vec3(width/2,height/2,depth/2);
				val a = d.map(x => max(x, 0.0)).magnitude;
				a + min(max(d.x, max(d.y, d.z)), 0)
			},
			Bounds(Vec3(-width/2,-height/2,-depth/2), Vec3(width, 0, 0), Vec3(0, height, 0), Vec3(0, 0, depth))
		)
	}
	
	def Box(b: Vec3): BoundedSDF = {
		return Box(b.x,b.y,b.z)
	}
	
	def Cylinder(r: Double, h: Double): BoundedSDF = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r, abs(p.y) - h);
			min(max(d.x,d.y),0.0) + d.map(x => max(x, 0)).magnitude
		}
		return BoundedSDF(
			func,
			Bounds(Vec3(-r,-h/2,-r), Vec3(2*r, 0, 0), Vec3(0, h, 0), Vec3(0, 0, 2*r))
		)
	}
	
	def Torus(r1: Double, r2: Double): BoundedSDF = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r1, p.y);
			d.magnitude - r2;
		}
		return BoundedSDF(
			func,
			Bounds(Vec3(-r1-r2,-r2,-r1-r2), Vec3(2*(r1+r2), 0, 0), Vec3(0, r2*2, 0), Vec3(0, 0, 2*(r1+r2)))
		)
	}
	
	def Ellipsoid(r: Vec3): BoundedSDF = {
		val func = (p: Vec3) => {
			val k0 = Vec3(p.x/r.x, p.y/r.y, p.z/r.z).magnitude;
			val k1 = Vec3(p.x/(r.x*r.x), p.y/(r.y*r.y), p.z/(r.z*r.z)).magnitude;
			k0*(k0-1)/k1;
		}
		return BoundedSDF(
			func,
			Bounds(-r, Vec3(2*r.x, 0, 0), Vec3(0, 2*r.y, 0), Vec3(0, 0, 2*r.z))
		)
	}
	def Ellipsoid(a: Double, b: Double, c: Double): BoundedSDF = {
		return Ellipsoid(Vec3(a,b,c))
	}
}