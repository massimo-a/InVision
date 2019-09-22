/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object Presets {
	def cornellBox(n: Int, w: Int, h: Int): Scene = {
		Scene(spp=n, width=w, height=h)++(
			Lighting(x=500,y=900,z=1000,size=30)
		)++(
			BoundedSdf.Box(250, 800, 250).rotateY(Math.PI/6).translate(300, 400, 1500),
			Diffuse(),
			(v: Vec3) => {Vec3(0.3, 0.3, 0.92)}
		)++(
			BoundedSdf.Sphere(200).translate(800, 200, 1800),
			Gloss(),
			(v: Vec3) => {Vec3(0.7, 0.2, 0.2)}
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
			Diffuse(0.0),
			(v: Vec3) => {Vec3(1.0, 1.0, 1.0)}
		)++(
			// Back wall
			Plane(Vec3(0, 0, -1), Vec3(1000, 1000, 2000)),
			Diffuse(),
			(v: Vec3) => {Vec3(0.7, 0.7, 0.7)}
		)
	}
	
	def emptyRoom(n: Int, w: Int, h: Int): Scene = {
		Scene(spp=n, width=w, height=h)++(
			Lighting(x=500,y=600,z=1000,size=30)
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
			(v: Vec3) => {Vec3(1.0, 1.0, 1.0)}
		)++(
			// Back wall
			Plane(Vec3(0, 0, -1), Vec3(1000, 1000, 2000)),
			Diffuse(),
			(v: Vec3) => {Vec3(0.7, 0.7, 0.7)}
		)
	}
	
	def lotsOfShapes(n: Int, w: Int, h: Int): Scene = {
		Scene(spp=n, width=w, height=h)++(
			Lighting(x=1000,y=1000,z=0,size=30)
		)++(
			BoundedSdf.Torus(200, 100).rotateX(Math.PI/4).translate(100, 300, 500),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 0, 0)}
		)++(
			BoundedSdf.Cylinder(50, 250).translate(500, 300, 500),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 0, 0)}
		)++(
			BoundedSdf.Ellipsoid(200, 50, 300).rotateX(Math.PI/4).rotateY(Math.PI/8).translate(1000, 300, 500),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 0, 0)}
		)++(
			Plane(Vec3(0, 1, 0), Vec3(0, 0, 0)),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 0)}
		)
	}
}