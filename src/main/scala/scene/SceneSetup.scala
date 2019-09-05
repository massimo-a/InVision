/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	val scene = Scene(spp=1)++(
		Lighting(x=100,y=800,z=1000,size=30)
	)++(
		ImpSurf.Hyperboloid(Vec3(700, 100, 1800), Vec3(500, 500, 500), Vec3(500, 200, 500)),
		Diffuse(0.5),
		(v: Vec3) => {
			Vec3(0.2, 0.30, 1.0)
		}
	)++(
		Plane(Vec3(1, 0, 0), Vec3(0, 0, 0)),
		Gloss(),
		(v: Vec3) => {
			Vec3(1, 0, 0)
		}
	)++(
		Plane(Vec3(0, 1, 0), Vec3(0, 0, 0)),
		Diffuse(),
		(v: Vec3) => {
			Vec3(1, 0, 1)
		}
	)++(
		Plane(Vec3(-1, 0, 0), Vec3(1000, 1000, 3000)),
		Diffuse(),
		(v: Vec3) => {
			Vec3(0, 0, 1)
		}
	)++(
		Plane(Vec3(0, -1, 0), Vec3(1000, 1000, 3000)),
		Diffuse(),
		(v: Vec3) => {
			Vec3(1, 1, 0)
		}
	)++(
		Plane(Vec3(0, 0, -1), Vec3(1000, 1000, 3000)),
		Diffuse(),
		(v: Vec3) => {
			Vec3(0, 1, 0)
		}
	)
}