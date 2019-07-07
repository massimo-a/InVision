/*
** Author:  Massimo Angelillo
*/

import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	val earth = Texture("world.jpg");
	val n = Noise();
	val scene = Scene(spp=10)++(
		Lighting(x=0,y=0,z=0,size=20)
	)++(
		Lighting(x=0,y=500,z=900,size=20)
	)++(
		Lighting(x=0,y=1000,z=0,size=20)
	)++(
		Lighting(x=1000,y=0,z=0,size=20)
	)++(
		BoundedSDF.SPHERE(120).translate(900,500,600),Transparency((v: Vec3) => {
			earth.wrap(v, Vec3(900, 500, 600));
		},0.0,1.5)
	)++(
		BoundedSDF.SPHERE(200).translate(400,800,600),Gloss((v: Vec3) => {
			val a = (v.y - 590)/400.0
			Vec3(1,0,0).lerp(Vec3(1,1,0),a)
		}, 0.25, 5.0)
	)++(
		BoundedSDF.SPHERE(90).translate(900,500,600),Gloss((v: Vec3) => {
			Vec3(1,0,0)
		},0.0)
	)
}