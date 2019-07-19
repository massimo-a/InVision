/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object SceneSetup {
	val board = Texture("textures/checkerboard.png");
	val scene = Scene(spp=3)++(
		Lighting(x=500,y=970,z=500,size=20,redEmission=3.0,blueEmission=3.0,greenEmission=3.0)
	)++(
		BoundedSDF.SPHERE(140).translate(800,140,860),
		Diffuse((v: Vec3) => {Vec3(1,0,0)},1.0)
	)++(
		BoundedSDF.BOX(200,400,200).rotateY(-Math.PI/6).translate(300,200,700),
		Gloss((v: Vec3) => {Vec3(0.7,0.7,0.7)},0.15,0.97)
	)++(
		Plane(Vec3(0,1,0),Vec3(0,0,0)),
		Gloss((v: Vec3) => {board.paste(v, Vec3(0,0,0), Vec3(1,0,0), 1024, 576)}, 0.0, 0.95)
	)++(
		Plane(Vec3(1,0,0),Vec3(0,0,0)),
		Diffuse((v: Vec3) => {Vec3(0,1,0)},1.0)
	)++(
		Plane(Vec3(0,-1,0),Vec3(1000,1000,1000)),
		Diffuse((v: Vec3) => {Vec3(0,1,1)},1.0)
	)++(
		Plane(Vec3(-1,0,0),Vec3(1000,1000,1000)),
		Diffuse((v: Vec3) => {Vec3(0,0,1)},1.0)
	)++(
		Plane(Vec3(0,0,-1),Vec3(1000,1000,1000)),
		Diffuse((v: Vec3) => {Vec3(1,1,0)},1.0)
	)
}