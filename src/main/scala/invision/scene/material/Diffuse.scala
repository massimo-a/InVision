package invision.scene.material

import invision.util.Vec3

final case class Diffuse(albedo: Double = 0.9) extends Material {
  def scatter(in: Vec3, n: Vec3): Vec3 = {
    val vec = sampleVectorOnSphere()
    vec * Math.signum(vec * n)
  }
}