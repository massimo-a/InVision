package invision.scene

import invision.geometry.Ray
import invision.geometry.intersectable.Intersectable
import invision.scene.material.Material
import invision.scene.renderable._
import invision.util.Vec3

import scala.collection.immutable.List
import scala.math.{max, random, Pi}

/**
 * Represents a scene with renderable objects and light sources.
 * @param renderables List of renderable objects in the scene.
 * @param lights List of lights in the scene.
 */
final case class World(renderables: List[Renderable] = List(), lights: List[Light] = List()) {
  /**
   * Adds a scene object to the world.
   * @param i The intersectable object being added.
   * @param m The objects material.
   * @param col The color of the object, as a function of points in space.
   * @return An updated world scene.
   */
  def ++(i: Intersectable, m: Material, col: Vec3=>Vec3): World = {
    World(SceneObject(i, m, col)::renderables, lights)
  }

  /**
   * Adds a scene object to the world.
   * @param i The intersectable object being added.
   * @param m The objects material.
   * @param col The color of the object, as a static value.
   * @return An updated world scene.
   */
  def ++(i: Intersectable, m: Material, col: Vec3): World = {
    World(SceneObject(i, m, col)::renderables, lights)
  }

  /**
   * Adds a light source to the world.
   * @param l The light source being added.
   * @return An updated world scene.
   */
  def ++(l: Light): World = {
    World(renderables, l::lights)
  }

  /**
   * Gets the renderable that intersects the ray and is closest to the ray.
   * @param ray The ray.
   * @return A pair of values representing the closest renderable and the point of intersection on that renderable,
   *         returns the NilRenderable object with point (0, 0, 0) if no renderable is hit along the ray.
   */
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

  /**
   * Gets the light source that intersects the ray and is closest to the ray.
   * @param ray The ray.
   * @return A pair of values representing the closest light and the point of intersection on that light,
   *         returns the NilLight object with point (0, 0, 0) if no light source is hit along the ray.
   */
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

  /**
   * Gets the color of an object at a specific point on that object by directly sampling light sources in the world.
   * @param obj The renderable whose color we are getting.
   * @param intersectPt The point on the renderable whose color we are getting.
   * @return A vector representing an rgb value.
   */
  def getColor(obj: Renderable, intersectPt: Vec3): Vec3 = {
    obj.color(intersectPt) ** lights.map(light => {
      val ptOnLight = light.samplePointOnLight((intersectPt - light.position).normalize())
      val lightDir = (ptOnLight - intersectPt).normalize()
      if(inLineOfSight(intersectPt, ptOnLight, light)) {
        light.color * max(0, obj.shape.getNormal(intersectPt)*lightDir)
      } else {
        Vec3()
      }
    }).fold(Vec3())((prev, v) => prev + v)
  }

  /**
   * Checks whether a point and a point on a light are in the line of sight of each other.
   * @param pt The initial point.
   * @param ptOnLight The point located on a light source.
   * @param light The light that is illuminating the point.
   * @return True if the light can illuminate the point.
   */
  def inLineOfSight(pt: Vec3, ptOnLight: Vec3, light: Light): Boolean = {
    val ray = Ray(pt + ((ptOnLight - pt).normalize() * 2), ptOnLight)
    val objHit = getClosestRenderable(ray)
    val lightHit = getClosestLight(ray)
    (objHit._1 == NilRenderable || ~(ptOnLight - pt) < ~(objHit._2 - pt)) &&
      (lightHit._1 == NilLight || lightHit._1 == light)
  }
}
