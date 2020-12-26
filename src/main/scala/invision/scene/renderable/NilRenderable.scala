package invision.scene.renderable

import invision.geometry.Intersectable
import invision.util.Vec3
import invision.scene.material.Material

case object NilRenderable extends Renderable {
  def shape: Intersectable = null
  def material: Material = null
  def color: Vec3 => Vec3 = null
}
