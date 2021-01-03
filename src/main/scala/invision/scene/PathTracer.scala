package invision.scene

import invision.geometry.Ray
import invision.scene.renderable.{NilLight, NilRenderable}
import invision.util.Vec3

import scala.math.random

final case class PathTracer(world: World, camera: Camera, spp: Int, width: Int, height: Int) extends Renderer {
  val toneMap: Double => Double = (x: Double) => {
    x/(x+1)
  }

  def trace(r: Ray): Vec3 = {
    val closestPoint = world.getClosestRenderable(r)
    val objHit = closestPoint._1
    val intersectPt = closestPoint._2
    val hitLight = world.getClosestLight(r)
    if(hitLight._1 != NilLight && (objHit == NilRenderable || ~(hitLight._2 - r.origin) < ~(intersectPt - r.origin)))
      return hitLight._1.color * (hitLight._1.getNormal(hitLight._2) * -r.direction)
    if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
    val col = world.getColor(objHit, intersectPt, r)
    val scatter = objHit.material.scatter(-r.direction, objHit.shape.getNormal(intersectPt))
    val newRay = Ray(intersectPt + scatter, intersectPt + scatter*3)
    val brdf = objHit.material.brdf(-r.direction, newRay.direction, objHit.shape.getNormal(intersectPt))
    if(random > brdf*0.9) return col
    val incoming = trace(newRay)
    (col + incoming*brdf).map(x => toneMap(x))
  }
}
