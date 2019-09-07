/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	// val noise = Value(17)
	// val hm = HeightMap.generate(1000, 4000)((x, y) => Noise.fractalize(noise, 5, x/800.0, y/800.0))
	// val terr = Terrain(hm, 250, 0, 0, -1000)
	val scene = Scene(spp=10)++(
		Lighting(x=100,y=800,z=1000,size=30)
	)++(
		Triangle(Vec3(0, 0, 0), Vec3(1000, 0, 0), Vec3(1000, 1000, 3000)),
		Transparency(0.9, 0.2),
		(v: Vec3) => {
			Vec3(1, 0, 0)
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

//(
//	ImpSurf.Hyperboloid(Vec3(700, 100, 1800), Vec3(500, 500, 500), Vec3(500, 200, 500)),
//	Diffuse(0.5),
//	(v: Vec3) => {
//		Vec3(0.2, 0.30, 1.0)
//	}
//)
//(
//	terr,
//	Diffuse(),
//	(v: Vec3) => {
//		Vec3(0.5, 0.5, 0.5)
//	}
//)