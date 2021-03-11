package invision.geometry

import invision.geometry.intersectable.{Intersectable, Triangle}
import invision.util.Vec3

case class TriangleMesh(vertices: Vector[Vec3], faces: Vector[Face]) extends Intersectable {
  val triangles: Vector[Triangle] = faces.map(face =>
    Triangle(
      vertices(face.vertexIndexOne),
      vertices(face.vertexIndexTwo),
      vertices(face.vertexIndexThree))
  )

  def intersectDistance(ray: Ray): Double = {
    val dists = triangles.map(x => x.intersectDistance(ray)).filter(x => x > 0)
    if(dists.nonEmpty) {
      dists.min
    } else -1
  }

  def getNormal(pt: Vec3): Vec3 = {
    triangles.find(tri => tri.pointInTriangle(pt)).get.getNormal(pt)
  }
}