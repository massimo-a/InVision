/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	val n = HeightMap.generate(1000, 500)((x, y) => Noise(255196).fractalPerlin(x/450.0, y/450.0, 5)*250)
	val scene = Scene(spp=4)++(
		Lighting(x=200,y=900,z=200,size=30)
	)++(
		Terrain(n, 260),
		Diffuse(),
		(v: Vec3) => {
			val t = v.y/250
			if(v.y < 0.2) {
				Vec3(1, 0, 0)
			} else {
				Vec3(1, 0, 0.5)*t + Vec3(0, 1, 1)*(1-t)
			}
		}
	)
}