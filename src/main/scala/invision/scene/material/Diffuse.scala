package invision.scene.material

import invision.util.Vec3

import scala.math.{cos, random, sin, sqrt, Pi}

final case class Diffuse(albedo: Double = 0.9) extends Material {
  def scatter(in: Vec3, n: Vec3): Vec3 = {
    val r = random
    val theta = random*2*Pi
    val randVec = Vec3(r*cos(theta), r*sin(theta), sqrt(1-r*r))
    (n + randVec * Math.signum(randVec * n)).normalize()
  }
  def brdf(in: Vec3, out: Vec3, n: Vec3): Double = albedo
}