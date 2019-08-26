/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	val n = HeightMap.generate(1000, 500)((x, y) => Noise(245196).perlin(x/200, y/200)*500)
	val scene = Scene(spp=2)++(
		Lighting(x=200,y=900,z=200,size=30)
	)++(
		Terrain(n, 600),
		Diffuse(),
		(v: Vec3) => {
			val t = v.x/1000
			Vec3(1, 0, 0.5)*t + Vec3(0, 1, 1)*(1-t)
		}
	)
}