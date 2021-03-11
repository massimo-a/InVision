package invision.util

import scala.math.{sqrt,cos,sin}

/** Defines a vector with all the needed vector operations.
 *  Vec2 is completely immutable and pure - no method alters the vector or its data,
 *  all methods return a new updated version of the vector
 *
 *  @author Massimo Angelillo
 *  @constructor create a new vector with an x and y coordinate values, which default to 0
 *  @param x the x coordinate value of the vector
 *  @param y the y coordinate value of the vector
 */
final case class Vec2(x: Double = 0, y: Double = 0) {
  /** Adds two vectors by adding each individual coordinate value
   */
  def +(v: Vec2): Vec2 = {
    Vec2(x + v.x, y + v.y)
  }

  /** Adds a scalar to the current vector object by adding it to each coordinate value
   */
  def +(a: Double): Vec2 = {
    Vec2(x + a, y + a)
  }

  /** Subtracts two vectors by subtracting each individual coordinate value
   */
  def -(v: Vec2): Vec2 = {
    Vec2(x - v.x, y - v.y)
  }

  /** Negates the coordinate values of the current vector
   */
  def unary_-(): Vec2 = {
    Vec2(-x, -y)
  }

  /** The dot product of two vectors
   */
  def *(v: Vec2): Double = {
    x*v.x + y*v.y
  }

  /** Multiplies the vector by a scalar value
   */
  def *(a: Double): Vec2 = {
    Vec2(x*a, y*a)
  }

  /** The hadamard product of two vector objects by multiplying through each coordinate value of the vectors
   */
  def **(v: Vec2): Vec2 = {
    Vec2(x*v.x, y*v.y)
  }

  /** The magnitude squared of this vector
   */
  def unary_~(): Double = {
    this*this
  }

  /** Normalizes the vector so that it has the same direction, but a magnitude of 1
   */
  def normalize(): Vec2 = {
    val mag = magnitude()
    if(mag < 0.00001 && mag > -0.00001) return Vec2()
    Vec2(x/mag, y/mag)
  }

  /**
   */
  def reflect(n: Vec2): Vec2 = {
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
  def map(func: Double => Double): Vec2 = {
    Vec2(func(x), func(y))
  }

  /**
   */
  def lerp(v: Vec2, t: Double): Vec2 = {
    Vec2(x*(1-t) + v.x*t, y*(1-t) + v.y*t)
  }

  /** The projection of this vector onto another vector.
   *  The projection is the component of this vector that is
   *  parallel (in the same direction) to the input vector
   */
  def proj(v: Vec2): Vec2 = {
    v*((this*v)/(~ this))
  }

  /** The rejection of this vector onto another vector.
   *  The rejection is the component of this vector that is
   *  perpendicular to the input vector
   */
  def rej(v: Vec2): Vec2 = {
    this - proj(v)
  }
}

object Vec2 {
  /**
   *  @param r the length of the vector
   *  @param theta the angle
   */
  def create(r: Double, theta: Double): Vec2 = {
    val x = r*cos(theta)
    val y = r*sin(theta)
    Vec2(x, y)
  }
}
