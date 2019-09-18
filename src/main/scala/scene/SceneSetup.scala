/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object Presets {
	def emptyRoom(n: Int): Scene = {
		Scene(spp=n)++(
			Lighting(x=100,y=800,z=1000,size=30)
		)++(
			// Left wall
			Plane(Vec3(1, 0, 0), Vec3(0, 0, 0)),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 0, 0)}
		)++(
			// Floor
			Plane(Vec3(0, 1, 0), Vec3(0, 0, 0)),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 0)}
		)++(
			// Right wall
			Plane(Vec3(-1, 0, 0), Vec3(1000, 1000, 2000)),
			Diffuse(),
			(v: Vec3) => {Vec3(0, 0, 1)}
		)++(
			// Ceiling
			Plane(Vec3(0, -1, 0), Vec3(1000, 1000, 2000)),
			Diffuse(),
			(v: Vec3) => {Vec3(1.2, 1.2, 1.2)}
		)++(
			// Back wall
			Plane(Vec3(0, 0, -1), Vec3(1000, 1000, 2000)),
			Diffuse(),
			(v: Vec3) => {Vec3(0.7, 0.7, 0.7)}
		)
	}
}