package invision.scene

import invision.geometry.Ray
import invision.geometry.intersectable.Intersectable
import invision.util.Vec3
import invision.scene.material.Material
import invision.scene.renderable.{BallLight, Light, NilLight, NilRenderable, Renderable, SceneObject}

import scala.collection.immutable.List
import scala.math.{max, random}

final case class World(renderables: List[Renderable] = List(), lights: List[Light] = List()) {
  def ++(i: Intersectable, m: Material, col: Vec3=>Vec3): World = {
    World(SceneObject(i, m, col)::renderables, lights)
  }

  def ++(i: Intersectable, m: Material, col: Vec3): World = {
    World(SceneObject(i, m, col)::renderables, lights)
  }

  def ++(l: Light): World = {
    World(renderables, l::lights)
  }

  def getClosestRenderable(ray: Ray): (Renderable, Vec3) = {
    renderables.foldLeft((NilRenderable: Renderable, Vec3()))((prev, rend) => {
      val dist = rend.shape.intersectDistance(ray)
      val distPrev = if(prev._1 != NilRenderable) {
        prev._1.shape.intersectDistance(ray)
      } else {
        -1
      }
      if(dist > 0) {
        if(dist < distPrev || distPrev < 0) {
          (rend, ray.origin + ray.direction*dist)
        } else {
          prev
        }
      } else {
        prev
      }
    })
  }

  def getClosestLight(ray: Ray): (Light, Vec3) = {
    lights.foldLeft((NilLight: Light, Vec3()))((prev, light) => {
      val dist = light.shape.intersectDistance(ray)
      val distPrev = if(prev._1 != NilLight) {
        prev._1.shape.intersectDistance(ray)
      } else {
        -1
      }
      if(dist > 0) {
        if(dist < distPrev || distPrev < 0) {
          (light, ray.origin + ray.direction*dist)
        } else {
          prev
        }
      } else {
        prev
      }
    })
  }

  def getColor(obj: Renderable, intersectPt: Vec3, ray: Ray): Vec3 = {
    obj.color(intersectPt) ** lights.foldLeft(Vec3())((prev, curr) => {
      if(inLineOfSight(intersectPt, curr)) {
        val lightDir = curr.position - intersectPt
        prev + curr.color * max(0, obj.shape.getNormal(intersectPt)*lightDir.normalize)
      } else prev
    })
  }

  def getLightSize(l: Light): Double = {
    l match {
      case BallLight(r, _, _, _) => r
      case NilLight => 0.0
    }
  }

  def inLineOfSight(pt: Vec3, l: Light): Boolean = {
    val lightP = l.position + Vec3(random*2-1, random*2-1, random*2-1)*getLightSize(l)
    val lightDir = (l.position - pt).normalize()
    val ray = Ray(pt + lightDir*2, lightP)
    val objHit = getClosestRenderable(ray)
    if(objHit._1 == NilRenderable || ~(lightP - pt) < ~(objHit._2 - pt)) return true
    false
  }
}
