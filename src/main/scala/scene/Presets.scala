/*
** Author:  Massimo Angelillo
*/

package raytracing.scene;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;
import scala.math.{Pi,sin,cos,abs}

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
	
	def pokeball(n: Int, w: Int, h: Int): Scene = {
		val pos = Vec3(500, 200, 700)
		val r = 200
		val half = BoundedSdf.Sphere(r)
			.intersect(BoundedSdf.Box(2*r, 2*r - 30, 2*r).translate(0, r, 0))
			.subtract(BoundedSdf.Sphere(50).translate(0, 0, -r))
		val center = BoundedSdf.Cylinder(30, 10).rotateX(Math.PI/2)
		Scene(spp=n, width=w, height=h)++(
			Lighting(x=500,y=900,z=200,size=30)
		)++(
			Plane(Vec3(0, 1, 0), Vec3(0, 0, 0)),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 0)}
		)++(
			half.translate(pos),
			Gloss(),
			(v: Vec3) => {Vec3(1, 0, 0)}
		)++(
			center.translate(pos - Vec3(0, 0, r)),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 1)}
		)++(
			half.rotateX(Math.PI).rotateY(Math.PI).translate(pos),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 1)}
		)++(
			BoundedSdf.Sphere(r - 5).translate(pos),
			Diffuse(0.0),
			(v: Vec3) => {Vec3(0,0,0)}
		)
	}
	
	def pokeballOpen(n: Int, w: Int, h: Int): Scene = {
		val pos = Vec3(500, 200, 700)
		val r = 200
		val angle = -Pi/8
		val half = BoundedSdf.Sphere(r)
			.intersect(BoundedSdf.Box(2*r, 2*r, 2*r).translate(0, r, 0))
			.subtract(BoundedSdf.Sphere(50).translate(0, 0, -r))
			.subtract(BoundedSdf.Sphere(r - 5))
		val center = BoundedSdf.Cylinder(30, 5).rotateX(Pi/2)
		Scene(spp=n, width=w, height=h)++(
			Lighting(x=500,y=900,z=200,size=30)
		)++(
			Lighting(x=500,y=200,z=700,size=100,visibility=true)
		)++(
			Plane(Vec3(0, 1, 0), Vec3(0, 0, 0)),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 0)}
		)++(
			half.rotateX(angle).translate(pos + Vec3(0, abs(sin(angle))*r + 10, r*(1 - abs(cos(angle))))),
			Gloss(),
			(v: Vec3) => {Vec3(1, 0, 0)}
		)++(
			center.translate(pos - Vec3(0, 0, r)),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 1)}
		)++(
			half.rotateX(Pi).rotateY(Pi).translate(pos),
			Diffuse(),
			(v: Vec3) => {Vec3(1, 1, 1)}
		)
	}
}