package invision.scene.material

import invision.util.Vec3

final case class Transparent(albedo: Double = 0.9, roughness: Double = 0.0) extends Material {
  def scatter(in: Vec3, n: Vec3): Vec3 = {
    val randVec = sampleVectorOnSphere()
    val bounce = -in
    (bounce + (randVec * roughness) * Math.signum(randVec * bounce)).normalize()
  }
}