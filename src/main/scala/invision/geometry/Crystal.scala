package invision.geometry

import invision.geometry.intersectable.Intersectable
import invision.util.Vec3

case class Crystal
(
  width: Double,
  height: Double,
  position: Vec3,
  orientation: Vector[Vec3] = Vector(Vec3(1), Vec3(0, 1), Vec3(0, 0, 1))) extends Intersectable {
  private val vertices = Vector(
    position,
    position + orientation(0)*width,
    position + (orientation(0) + orientation(2))*0.5*width + orientation(1)*height,
    position + orientation(2)*width,
    position + (orientation(0) + orientation(2))*width,
    position + (orientation(0) + orientation(2))*0.5*width - orientation(1)*height)

  private val faces = Face.from(
    Seq(
      0, 1, 2,
      0, 2, 3,
      3, 2, 4,
      4, 2, 1,
      1, 0, 5,
      3, 5, 0,
      4, 5, 3,
      1, 5, 4))

  private val mesh = TriangleMesh(vertices, faces)

  def intersectDistance(ray: Ray): Double = {
    mesh.intersectDistance(ray)
  }

  def getNormal(pt: Vec3): Vec3 = {
    mesh.getNormal(pt)
  }
}
