package invision.scene.renderable

import invision.geometry.intersectable.{Intersectable, Sphere}
import invision.util.Vec3

import scala.math.{random, Pi}

final case class BallLight(size: Double, x: Double, y: Double, z: Double, color: Vec3 = Vec3(1, 1, 1)) extends Light {
  def position: Vec3 = Vec3(x, y, z)
  def shape: Intersectable = Sphere(size, position)
  def samplePointOnLight(direction: Vec3): Vec3 = {
    val delta = Vec3.create(size, random * Pi, random * 2 * Pi)
    if(direction*delta < 0) position - delta else position + delta
  }
}