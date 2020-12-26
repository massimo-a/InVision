package invision.scene.renderable

import invision.geometry.Intersectable
import invision.util.Vec3
import invision.scene.material.Material

/**
 *  @author Massimo Angelillo
 */
trait Renderable {
  def shape: Intersectable
  def material: Material
  def color: Vec3 => Vec3
}