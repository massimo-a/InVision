/*
** Author : Massimo Angelillo
**
** Defines a vector object with all the needed
** vector operations. Vec3 is completely immutable and pure - no method
** alters the vector or its data, all methods return a new updated
** version of the vector
*/

package raytracing.util;
import scala.math.{sqrt,cos,sin};

case class Vec3(x: Double = 0, y: Double = 0, z: Double = 0) {
	def +(v: Vec3): Vec3 = {
		Vec3(x + v.x, y + v.y, z + v.z);
	}
	def +(a: Double): Vec3 = {
		Vec3(x + a, y + a, z + a);
	}
	def -(v: Vec3): Vec3 = {
		Vec3(x - v.x, y - v.y, z - v.z);
	}
	def unary_-(): Vec3 = {
		Vec3(-x, -y, -z);
	}
	//dot product
	def *(v: Vec3): Double = {
		x*v.x + y*v.y + z*v.z;
	}
	//scaling
	def *(a: Double): Vec3 = {
		Vec3(x*a, y*a, z*a);
	}
	//hadamard product
	def **(v: Vec3): Vec3 = {
		Vec3(x*v.x, y*v.y, z*v.z);
	}
	//cross product
	def ^(v: Vec3): Vec3 = {
		Vec3(y*v.z-z*v.y, z*v.x-x*v.z, x*v.y-y*v.x);
	}
	//magnitude squared
	def unary_~(): Double = {
		this*this
	}
	def normalize(): Vec3 = {
		val mag = magnitude
		if(mag < 0.00001 && mag > -0.00001) return Vec3()
		Vec3(x/mag, y/mag, z/mag);
	}
	def reflect(n: Vec3): Vec3 = {
		-(this-(n*(2*(this*n))));
	}
	def rotate(n: Vec3, ang: Double): Vec3 = {
		this*cos(ang) + (this^n)*sin(ang) + n*((this*n)*(1-cos(ang)));
	}
	def magnitude(): Double = {
		sqrt(~ this);
	}
	def map(func: (Double) => (Double)): Vec3 = {
		return Vec3(func(x), func(y), func(z));
	}
	def lerp(v: Vec3, t: Double): Vec3 = {
		return Vec3(x*(1-t) + v.x*t, y*(1-t) + v.y*t, z*(1-t) + v.z*t);
	}
	/*
	** The projection and the rejection of this vector onto a vector v
	**
	** The projection is the component of this vector that is
	** in the same direction as v, the rejection is the component
	** of this vector that is perpendicular to v.
	** NOTE : proj(v) + rej(v) = this
	*/
	def proj(v: Vec3): Vec3 = {
		v*((this*v)/(~ this));
	}
	def rej(v: Vec3): Vec3 = {
		this-(proj(v));
	}
}

object Vec3 {
	/*
	** @theta : the polar angle, angle of the vector with respect to the positive up-axis (often-times z)
	** @phi : the azimuthal angle, angle of the vector with respect to an axis perpendicular to the polar axis (often-times x)
	** @r : the length of the vector
	*/
	def create(r: Double, theta: Double, phi: Double): Vec3 = { 
		val x = r*sin(theta)*sin(phi);
		val y = r*sin(theta)*cos(phi);
		val z = r*cos(theta);
		Vec3(x, y, z);
	}
	
	def orthonormalize(v1: Vec3, v2: Vec3): Array[Vec3] = {
		val base1 = v1.normalize;
		val base2 = (v2 - (base1.proj(v2))).normalize;
		Array(base1, base2, (base1^base2).normalize);
	}
}