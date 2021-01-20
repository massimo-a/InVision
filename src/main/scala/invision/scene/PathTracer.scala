package invision.scene

import invision.geometry.Ray
import invision.geometry.intersectable.Intersectable
import invision.scene.renderable.{NilLight, NilRenderable}
import invision.util.Vec3

import scala.math.{random, Pi}

final case class PathTracer(world: World, camera: Camera, spp: Int, width: Int, height: Int) extends Renderer {
  val toneMap: Double => Double = (x: Double) => {
    x/(x+1)
  }

  def trace(r: Ray): Vec3 = {
    val closestRenderable = world.getClosestRenderable(r)
    val objHit = closestRenderable._1
    val intersectPt = closestRenderable._2
    val hitLight = world.getClosestLight(r)
    if(hitLight._1 != NilLight && (objHit == NilRenderable || ~(hitLight._2 - r.origin) < ~(intersectPt - r.origin)))
      return hitLight._1.color * (hitLight._1.getNormal(hitLight._2) * -r.direction)
    if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
    val col = world.getColor(objHit, intersectPt)
    val scatter = objHit.material.scatter(-r.direction, objHit.shape.getNormal(intersectPt))
    val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3)
    if(random > objHit.material.albedo*0.9) return col
    val incoming = trace(newRay)
    (col + incoming*objHit.material.albedo).map(toneMap)
  }

  /**
   * For sampling a volume for volume rendering (smokey and transparent materials).
   * @param shape shape being sampled.
   * @param ray ray.
   * @return ray with a new direction exiting the shape.
   */
  @scala.annotation.tailrec
  def sample(shape: Intersectable, ray: Ray): Ray = {
    val tmax = shape.intersectDistance(ray)
    val s = -0.01*Math.log(random)
    if(s > tmax) {
      Ray(ray.origin + ray.direction*(tmax+1), ray.origin + ray.direction*(tmax+2))
    } else {
      val newDir = Vec3.create(1, random() * Pi, random() * 2 * Pi)
      val newRay = Ray(ray.origin + ray.direction * s, ray.origin + ray.direction * s + newDir)
      sample(shape, newRay)
    }
  }
}
