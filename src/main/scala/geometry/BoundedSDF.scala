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
final case class BoundedSdf(
	equation: Vec3 => Double,
	boundingBox: Bounded
) extends SurfaceMarcher {
	/*
	** union merges two BoundedSDFs together. Visually there is no
	** difference between keeping the BoundedSDFs separate or unioning
	** them
	*/
	def union(surf: BoundedSdf): BoundedSdf = {
		val func = (v: Vec3) => {
			min(equation(v), surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSdf(func, bounds);
	}
	
	/*
	** The same as a normal union, but the point of contact between
	** the two BoundedSDFs has a smooth transition, determined by k.
	** The higher the value of k, the 'smoother' the transition
	*/
	def smoothUnion(surf: BoundedSdf, k: Double): BoundedSdf = {
		val func = (v: Vec3) => {
			val h = max(k - abs(equation(v) - surf.equation(v)), 0.0)/k;
			min(equation(v), surf.equation(v)) - h*h*k*0.25;
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSdf(func, bounds);
	}
	
	// Takes two BoundedSDFs and returns the BoundedSdf that is contained by both
	def intersect(surf: BoundedSdf): BoundedSdf = {
		val func = (v: Vec3) => {
			max(equation(v), surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSdf(func, bounds);
	}
	
	def smoothIntersect(surf: BoundedSdf, k: Double): BoundedSdf = {
		val func = (v: Vec3) => {
			val h = max(k - abs(equation(v) - surf.equation(v)), 0.0)/k;
			max(equation(v), surf.equation(v)) - h*h*k*0.25;
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSdf(func, bounds);
	}
	
	/*
	** Takes two BoundedSDFs and returns the BoundedSdf
	** that is contained by the first, but not the second
	** NOTE: This function is not commutative,
	** i.e a.subtract(b) =/= b.subtract(a)
	*/
	def subtract(surf: BoundedSdf): BoundedSdf = {
		val func = (v: Vec3) => {
			max(equation(v), -surf.equation(v));
		}
		val bounds = boundingBox.merge(surf.boundingBox);
		return BoundedSdf(func, bounds);
	}
	
	/*
	** These methods allow translating and rotating
	** a BoundedSdf in 3D space. The rotating methods take
	** the angle as a radian, not a degree.
	** When the shape is rotated, it is rotated about the axis
	** according to the right-hand rule
	*/
	def translate(x: Double, y: Double, z: Double): BoundedSdf = {
		return BoundedSdf(v => (equation(v - Vec3(x, y, z))), boundingBox.translate(x, y, z))
	}
	def translate(u: Vec3): BoundedSdf = {
		return BoundedSdf(v => (equation(v - u)), boundingBox.translate(u.x, u.y, u.z))
	}
	
	/*
	** The rotate functions should always be called BEFORE
	** the translate function, for it to work the way you think it will
	*/
	def rotateX(rad: Double): BoundedSdf = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(v.x, c*v.y - s*v.z, s*v.y + c*v.z)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return BoundedSdf(func, boundingBox.rotateWith(rotatePoint));
	}
	def rotateY(rad: Double): BoundedSdf = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(c*v.x + s*v.z, v.y, c*v.z - s*v.x)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return BoundedSdf(func, boundingBox.rotateWith(rotatePoint));
	}
	def rotateZ(rad: Double): BoundedSdf = {
		val c = cos(rad);
		val s = sin(rad);
		val rotatePoint = (v: Vec3) => {
			Vec3(c*v.x - s*v.y, c*v.y + s*v.x, v.z)
		}
		val func = (v: Vec3) => {
			equation(rotatePoint(v));
		}
		return BoundedSdf(func, boundingBox.rotateWith(rotatePoint));
	}
	def rotate(rad: Vec3): BoundedSdf = {
		return this.rotateX(rad.x).rotateY(rad.y).rotateZ(rad.z);
	}
	def rotate(x: Double, y: Double, z: Double): BoundedSdf = {
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

object BoundedSdf {
	def Sphere(r: Double): BoundedSdf = {
		return BoundedSdf(
			(v: Vec3) => {v.magnitude - r},
			BoundingBox(Vec3(-r-10, -r-10, -r-10), Vec3(2*r+20, 0, 0), Vec3(0, 2*r+20, 0), Vec3(0, 0, 2*r+20))
		)
	}
	
	def Box(width: Double, height: Double, depth: Double): BoundedSdf = {
		return BoundedSdf(
			(v: Vec3) => {
				val d = (v).map(x => abs(x)) - Vec3(width/2,height/2,depth/2);
				val a = d.map(x => max(x, 0.0)).magnitude;
				a + min(max(d.x, max(d.y, d.z)), 0)
			},
			BoundingBox(Vec3(-width/2,-height/2,-depth/2), Vec3(width, 0, 0), Vec3(0, height, 0), Vec3(0, 0, depth))
		)
	}
	
	def Box(b: Vec3): BoundedSdf = {
		return Box(b.x,b.y,b.z)
	}
	
	def Cylinder(r: Double, h: Double): BoundedSdf = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r, abs(p.y) - h/2);
			min(max(d.x,d.y),0.0) + d.map(x => max(x, 0)).magnitude
		}
		return BoundedSdf(
			func,
			BoundingBox(Vec3(-r-5,-h/2-5,-r-5), Vec3(2*r+10, 0, 0), Vec3(0, h+10, 0), Vec3(0, 0, 2*r+10))
		)
	}
	
	def Torus(r1: Double, r2: Double): BoundedSdf = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r1, p.y);
			d.magnitude - r2;
		}
		return BoundedSdf(
			func,
			BoundingBox(Vec3(-r1-2*r2,-r2,-r1-2*r2), Vec3(2*(r1+2*r2), 0, 0), Vec3(0, r2*2, 0), Vec3(0, 0, 2*(r1+2*r2)))
		)
	}
	
	def Ellipsoid(r: Vec3): BoundedSdf = {
		val func = (p: Vec3) => {
			val k0 = Vec3(p.x/r.x, p.y/r.y, p.z/r.z).magnitude;
			val k1 = Vec3(p.x/(r.x*r.x), p.y/(r.y*r.y), p.z/(r.z*r.z)).magnitude;
			k0*(k0-1)/k1;
		}
		return BoundedSdf(
			func,
			BoundingBox(-r, Vec3(2*r.x, 0, 0), Vec3(0, 2*r.y, 0), Vec3(0, 0, 2*r.z))
		)
	}
	
	def Ellipsoid(a: Double, b: Double, c: Double): BoundedSdf = {
		return Ellipsoid(Vec3(a,b,c))
	}
	
	def SolidAngle(r: Double, angle: Double): BoundedSdf = {
		val clamp = (x: Double, a: Double, b: Double) => {
			if(x < a) a
			if(x > b) b
			x
		}
		val func = (p: Vec3) => {
			val c = Vec3(Math.sin(angle), Math.cos(angle))
			val q = Vec3(Vec3(p.x, p.z).magnitude, p.y)
			val l = q.magnitude - r;
			val m = (q - c*clamp(q*c, 0.0, r)).magnitude;
			max(l, m*Math.signum(c.y*q.x-c.x*q.y));
		}
		return BoundedSdf(
			func,
			BoundingBox(Vec3(-r, -r, -r), Vec3(2*r, 0, 0), Vec3(0, 2*r, 0), Vec3(0, 0, 2*r))
		)
	}
}