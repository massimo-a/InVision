package invision.scene.renderable

import invision.geometry.Intersectable
import invision.util.Vec3
import invision.scene.material.Material

final case class SceneObject(shape: Intersectable, material: Material, color: Vec3=>Vec3) extends Renderable

case object SceneObject {
  def apply(sh: Intersectable, mat: Material, col: Vec3): SceneObject = {
    SceneObject(sh, mat, (_: Vec3) => col)
  }
}