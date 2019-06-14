/*
** Author:  Massimo Angelillo
*/

package raytracing.geometry;
import raytracing.util.Vec3;
import scala.math.{abs,max,min,cos,sin};

/* 
** An SDF takes a point in space (given as a vector) and returns
** the distance to the closest point on the SDF. It is 'signed'
** because it returns positive distance when the point is outside the
** SDF and negative when inside the SDF.
*/
case class BoundedSDF(
	equation: Vec3 => Double,
	OOBB: Array[Vec3]
) extends Surface with Bounded {
	private val epsilon = 1;
	val minimum = Vec3(OOBB.minBy(_.x).x, OOBB.minBy(_.y).y, OOBB.minBy(_.z).z)
	val maximum = Vec3(OOBB.maxBy(_.x).x, OOBB.maxBy(_.y).y, OOBB.maxBy(_.z).z)
	
	def distort(func: (Vec3) => (Double)): BoundedSDF = {
		return BoundedSDF(
			(v: Vec3) => {equation(v) + func(v)},
			OOBB
		)
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
		val bounds = merge(minimum, maximum, surf.minimum, surf.maximum);
		val oobb = getOOBB(bounds._1, bounds._2);
		return BoundedSDF(func, oobb);
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
		val bounds = merge(minimum, maximum, surf.minimum, surf.maximum);
		val oobb = getOOBB(bounds._1, bounds._2);
		return BoundedSDF(func, oobb);
	}
	
	// Takes two BoundedSDFs and returns the BoundedSDF that is contained by both
	def intersect(surf: BoundedSDF): BoundedSDF = {
		val func = (v: Vec3) => {
			max(equation(v), surf.equation(v));
		}
		val bounds = merge(minimum, maximum, surf.minimum, surf.maximum);
		val oobb = getOOBB(bounds._1, bounds._2);
		return BoundedSDF(func, oobb);
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
		val bounds = merge(minimum, maximum, surf.minimum, surf.maximum);
		val oobb = getOOBB(bounds._1, bounds._2);
		return BoundedSDF(func, oobb);
	}
	
	/*
	** These methods allow translating and rotating
	** a BoundedSDF in 3D space. The rotating methods take
	** the angle as a radian, not a degree.
	** When the shape is rotated, it is rotated about the axis
	** according to the right-hand rule
	*/
	def translate(x: Double, y: Double, z: Double): BoundedSDF = {
		return BoundedSDF(v => (equation(v - Vec3(x, y, z))), OOBB.map(_ + Vec3(x, y, z)))
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
		val oobb = OOBB.map(x => rotatePoint(x))
		return BoundedSDF(func, oobb);
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
		val oobb = OOBB.map(x => rotatePoint(x))
		return BoundedSDF(func, oobb);
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
		val oobb = OOBB.map(x => rotatePoint(x))
		return BoundedSDF(func, oobb);
	}
	def stretchBoundingBox(v: Vec3): BoundedSDF = {
		val oobb = getOOBB(minimum - v, maximum + v);
		return BoundedSDF(equation, oobb)
	}
	private def findRoot(func: Double => Double, pt: Double, dist: Double): Double = {
		if(dist < 0) return -1;
		if(func(pt) < 1 && func(pt) > -1) return pt;
		return findRoot(func, pt + abs(func(pt)), dist - abs(func(pt)));
	}
	def intersectDistance(r: Ray): Double = {
		if(hitBox(r)) {
			val pts = intersections(r);
			return findRoot((x: Double) => {equation(r.equation(x))}, pts._1, (pts._2 - pts._1));
		} else return -1;
	}
}

object BoundedSDF {
	def SPHERE(r: Double): BoundedSDF = {
		return BoundedSDF(
			(v: Vec3) => {v.magnitude - r},
			Array(
				Vec3(-r,-r,-r),Vec3(r,-r,-r),Vec3(r,-r,r),Vec3(-r,-r,r),
				Vec3(-r,r,-r),Vec3(r,r,-r),Vec3(r,r,r),Vec3(-r,r,r)
			)
		)
	}
	
	def BOX(width: Double, height: Double, depth: Double): BoundedSDF = {
		return BoundedSDF(
			(v: Vec3) => {
				val d = (v).map(x => abs(x)) - Vec3(width/2,height/2,depth/2);
				val a = d.map(x => max(x, 0.0)).magnitude;
				a + min(max(d.x, max(d.y, d.z)), 0)
			},
			Array(
				-Vec3(width/2,height/2,depth/2),Vec3(width/2,-height/2,-depth/2),
				Vec3(width/2,-height/2,depth/2),Vec3(-width/2,-height/2,depth/2),
				Vec3(-width/2,height/2,-depth/2),Vec3(width/2,height/2,-depth/2),
				Vec3(width/2,height/2,depth/2),Vec3(-width/2,height/2,depth/2)
			)
		)
	}
	
	def BOX(b: Vec3): BoundedSDF = {
		return BOX(b.x,b.y,b.z)
	}
	
	def CYLINDER(r: Double, h: Double): BoundedSDF = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r, abs(p.y) - h);
			min(max(d.x,d.y),0.0) + d.map(x => max(x, 0)).magnitude
		}
		return BoundedSDF(
			func,
			Array(
				Vec3(-r,-h,-r),Vec3(r,-h,-r),Vec3(r,-h,r),Vec3(-r,-h,r),
				Vec3(-r,h,-r),Vec3(r,h,-r),Vec3(r,h,r),Vec3(-r,h,r)
			)
		)
	}
	
	def TORUS(r1: Double, r2: Double): BoundedSDF = {
		val func = (p: Vec3) => {
			val d = Vec3(Vec3(p.x, p.z).magnitude - r1, p.y);
			d.magnitude - r2;
		}
		return BoundedSDF(
			func,
			Array(
				Vec3(-r1-r2,-r2,-r1-r2),Vec3(r1+r2,-r2,-r1-r2),Vec3(r1+r2,-r2,r1+r2),Vec3(-r1-r2,-r2,r1+r2),
				Vec3(-r1-r2,r2,-r1-r2),Vec3(r1+r2,r2,-r1-r2),Vec3(r1+r2,r2,r1+r2),Vec3(-r1-r2,r2,r1+r2)
			)
		)
	}
	
	def ELLIPSOID(r: Vec3): BoundedSDF = {
		val func = (p: Vec3) => {
			val k0 = Vec3(p.x/r.x, p.y/r.y, p.z/r.z).magnitude;
			val k1 = Vec3(p.x/(r.x*r.x), p.y/(r.y*r.y), p.z/(r.z*r.z)).magnitude;
			k0*(k0-1)/k1;
		}
		return BoundedSDF(
			func,
			Array(
				-r,Vec3(r.x,-r.y,-r.z),Vec3(r.x,-r.y,r.z),Vec3(-r.x,-r.y,r.z),
				Vec3(-r.x,r.y,-r.z),Vec3(r.x,r.y,-r.z),r,Vec3(-r.x,r.y,r.z)
			)
		)
	}
	def ELLIPSOID(a: Double, b: Double, c: Double): BoundedSDF = {
		return ELLIPSOID(Vec3(a,b,c))
	}
}