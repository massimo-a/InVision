package raytracing.util;
import scala.math.{sqrt,cos,sin};

/** Defines a vector with all the needed vector operations.
 *  Vec3 is completely immutable and pure - no method alters the vector or its data,
 *  all methods return a new updated version of the vector
 *  
 *  @author Massimo Angelillo
 *  
 *  @constructor create a new vector with an x, y and z coordinate values, which default to 0
 *  @param x the x coordinate value of the vector
 *  @param y the y coordinate value of the vector
 *  @param z the z coordinate value of the vector
 */
final case class Vec3(x: Double = 0, y: Double = 0, z: Double = 0) {
	/** Adds two vectors by adding each individual coordinate value
	 */
	def +(v: Vec3): Vec3 = {
		Vec3(x + v.x, y + v.y, z + v.z);
	}
	
	/** Adds a scalar to the current vector object by adding it to each coordinate value
	 */
	def +(a: Double): Vec3 = {
		Vec3(x + a, y + a, z + a);
	}
	
	/** Subtracts two vectors by subtracting each individual coordinate value
	 */
	def -(v: Vec3): Vec3 = {
		Vec3(x - v.x, y - v.y, z - v.z);
	}
	
	/** Negates the coordinate values of the current vector
	 */
	def unary_-(): Vec3 = {
		Vec3(-x, -y, -z);
	}
	
	/** The dot product of two vectors
	 */
	def *(v: Vec3): Double = {
		x*v.x + y*v.y + z*v.z;
	}
	
	/** Multiplies the vector by a scalar value
	 */
	def *(a: Double): Vec3 = {
		Vec3(x*a, y*a, z*a);
	}
	
	/** The hadamard product of two vector objects by multiplying through each coordinate value of the vectors
	 */
	def **(v: Vec3): Vec3 = {
		Vec3(x*v.x, y*v.y, z*v.z);
	}
	
	/** The cross product of two vectors
	 */
	def ^(v: Vec3): Vec3 = {
		Vec3(z*v.y-y*v.z, x*v.z-z*v.x, y*v.x-x*v.y);
	}
	
	/** The magnitude squared of this vector
	 */
	def unary_~(): Double = {
		this*this
	}
	
	/** Normalizes the vector so that it has the same direction, but a magnitude of 1
	 */
	def normalize(): Vec3 = {
		val mag = magnitude
		if(mag < 0.00001 && mag > -0.00001) return Vec3()
		Vec3(x/mag, y/mag, z/mag);
	}
	
	/** 
	 */
	def reflect(n: Vec3): Vec3 = {
		-(this-(n*(2*(this*n))));
	}
	
	/** 
	 */
	def rotate(n: Vec3, ang: Double): Vec3 = {
		this*cos(ang) + (this^n)*sin(ang) + n*((this*n)*(1-cos(ang)));
	}
	
	/** 
	 *
	 */
	def magnitude(): Double = {
		sqrt(~ this);
	}
	
	/** 
	 */
	def map(func: (Double) => (Double)): Vec3 = {
		return Vec3(func(x), func(y), func(z));
	}
	
	/** 
	 */
	def lerp(v: Vec3, t: Double): Vec3 = {
		return Vec3(x*(1-t) + v.x*t, y*(1-t) + v.y*t, z*(1-t) + v.z*t);
	}
	
	/** The projection of this vector onto another vector.
	 *  The projection is the component of this vector that is
	 *  parallel (in the same direction) to the input vector
	 */
	def proj(v: Vec3): Vec3 = {
		v*((this*v)/(~ this));
	}
	
	/** The rejection of this vector onto another vector.
	 *  The rejection is the component of this vector that is
	 *  perpendicular to the input vector
	 */
	def rej(v: Vec3): Vec3 = {
		this-(proj(v));
	}
}

object Vec3 {
	/** 
	 *  @param theta the polar angle, angle of the vector with respect to the positive up-axis (often-times z)
	 *  @param phi the azimuthal angle, angle of the vector with respect to an axis perpendicular to the polar axis (often-times x)
	 *  @param r the length of the vector
	 */
	def create(r: Double, theta: Double, phi: Double): Vec3 = { 
		val x = r*sin(theta)*sin(phi);
		val y = r*sin(theta)*cos(phi);
		val z = r*cos(theta);
		Vec3(x, y, z);
	}
}