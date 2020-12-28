package invision.scene

import invision.geometry.Ray
import invision.scene.renderable.{NilLight, NilRenderable}
import invision.util.Vec3

final case class FastTracer(world: World, camera: Camera, spp: Int, width: Int, height: Int) extends Renderer {
  def trace(ray: Ray): Vec3 = {
    val closestPoint = world.getClosestRenderable(ray)
    val objHit = closestPoint._1
    val intersectPt = closestPoint._2
    val hitLight = world.getClosestLight(ray)
    if(hitLight._1 != NilLight) return hitLight._1.color
    if(objHit == NilRenderable) return Vec3(0.2, 0.2, 0.2)
    world.getColor(objHit, intersectPt, ray)
  }
}
