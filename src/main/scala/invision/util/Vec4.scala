package invision.util

import scala.math.sqrt

/** Defines a vector with all the needed vector operations.
 *  Vec4 is completely immutable and pure - no method alters the vector or its data,
 *  all methods return a new updated version of the vector
 *
 *  @author Massimo Angelillo
 *  @constructor create a new vector with an x, y, z and w coordinate values, which default to 0
 *  @param x the x coordinate value of the vector
 *  @param y the y coordinate value of the vector
 *  @param z the z coordinate value of the vector
 *  @param w the w coordinate value of the vector
 */
final case class Vec4(x: Double = 0, y: Double = 0, z: Double = 0, w: Double = 0) {
  /** Adds two vectors by adding each individual coordinate value
   */
  def +(v: Vec4): Vec4 = {
    Vec4(x + v.x, y + v.y, z + v.z, w + v.w)
  }

  /** Adds a scalar to the current vector object by adding it to each coordinate value
   */
  def +(a: Double): Vec4 = {
    Vec4(x + a, y + a, z + a, w + a)
  }

  /** Subtracts two vectors by subtracting each individual coordinate value
   */
  def -(v: Vec4): Vec4 = {
    Vec4(x - v.x, y - v.y, z - v.z, w - v.w)
  }

  /** Negates the coordinate values of the current vector
   */
  def unary_-(): Vec4 = {
    Vec4(-x, -y, -z, -w)
  }

  /** The dot product of two vectors
   */
  def *(v: Vec4): Double = {
    x*v.x + y*v.y + z*v.z + w*v.w
  }

  /** Multiplies the vector by a scalar value
   */
  def *(a: Double): Vec4 = {
    Vec4(x*a, y*a, z*a, w*a)
  }

  /** The hadamard product of two vector objects by multiplying through each coordinate value of the vectors
   */
  def **(v: Vec4): Vec4 = {
    Vec4(x*v.x, y*v.y, z*v.z, w*v.w)
  }

  /** The magnitude squared of this vector
   */
  def unary_~(): Double = {
    this*this
  }

  /** Normalizes the vector so that it has the same direction, but a magnitude of 1
   */
  def normalize(): Vec4 = {
    val mag = magnitude()
    if(mag < 0.00001 && mag > -0.00001) return Vec4()
    Vec4(x/mag, y/mag, z/mag, w/mag)
  }

  /**
   */
  def reflect(n: Vec4): Vec4 = {
    -(this-(n*(2*(this*n))))
  }

  /**
   *
   */
  def magnitude(): Double = {
    sqrt(~ this)
  }

  /**
   */
  def map(func: Double => Double): Vec4 = {
    Vec4(func(x), func(y), func(z), func(w))
  }

  /**
   */
  def lerp(v: Vec4, t: Double): Vec4 = {
    Vec4(x*(1-t) + v.x*t, y*(1-t) + v.y*t, z*(1-t) + v.z*t, w*(1-t) + v.w*t)
  }

  /** The projection of this vector onto another vector.
   *  The projection is the component of this vector that is
   *  parallel (in the same direction) to the input vector
   */
  def proj(v: Vec4): Vec4 = {
    v*((this*v)/(~ this))
  }

  /** The rejection of this vector onto another vector.
   *  The rejection is the component of this vector that is
   *  perpendicular to the input vector
   */
  def rej(v: Vec4): Vec4 = {
    this - proj(v)
  }
}
