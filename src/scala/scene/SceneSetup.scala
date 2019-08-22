/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	val text = Texture("textures/checkerboard.png")
	val scene = Scene(spp=4)++(
		Lighting(x=200,y=900,z=200,size=30)
	)++(
		Plane(Vec3(0,1,0),Vec3(0,0,0)),
		Diffuse(),
		(v: Vec3) => {text.paste(v, Vec3(0, 0, 0), Vec3(1, 0, 0))}
	)++(
		BoundedSDF.Sphere(200).translate(300, 500, 700)
			.subtract(BoundedSDF.Cylinder(260, 50).rotateZ(Math.PI/4).translate(300, 500, 700))
			.subtract(BoundedSDF.Cylinder(260, 50).rotateZ(3*Math.PI/4).translate(300, 500, 700)),
		Diffuse(),
		(v: Vec3) => {
			val t = (v.x - 100)/400
			Vec3(1, 0, 0.5)*t + Vec3(0, 1, 1)*(1-t)
		}
	)
}