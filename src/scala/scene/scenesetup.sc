/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	val scene = Scene(spp=4)++(
		Lighting(x=0,y=0,z=0,size=20)
	)++(
		Lighting(x=0,y=1000,z=0,size=20)
	)++(
		Lighting(x=1000,y=1000,z=0,size=20)
	)++(
		Lighting(x=1000,y=0,z=0,size=20)
	)++(
		BoundedSDF.SPHERE(140).translate(500,500,700),Scatter((v: Vec3) => {
			Vec3(1,0,0)
		},1.0,0.4,0.4)
	)++(
		BoundedSDF.BOX(30,500,500).rotateY(-Math.PI/4).translate(300,500,750),Diffuse((v: Vec3) => {
			Vec3(0.5,0.5,0.5)
		},1.0)
	)
}