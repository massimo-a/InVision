import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object Room {
	def mug(r: Double, h: Double, g: Double): Surface = {
		val handle = (a: Double, b: Double) => {
			Surface.TORUS(a, b).
			rotateX(Math.PI/2).
			intersect(Surface.BOX(Vec3(a+b,a+b,b)).translate(a+b,0,0))
		}
		val body = (width: Double, height: Double, gap: Double) => {
			Surface.CYLINDER(width, height).
			subtract(Surface.CYLINDER(width-gap, height-gap).translate(0,gap+2,0))
		}
		return body(r, h, g).smoothUnion(handle(h/1.5, g).translate(r, 0, 0), 10);
	}
	def table(l: Double, w: Double, h: Double, g: Double): Surface = {
		val leg1 = Surface.BOX(g, h, g).translate(l/2, 0, w/2);
		val leg2 = Surface.BOX(g, h, g).translate(l/2, 0, -w/2);
		val leg3 = Surface.BOX(g, h, g).translate(-l/2, 0, w/2);
		val leg4 = Surface.BOX(g, h, g).translate(-l/2, 0, -w/2);
		val body = Surface.BOX(l, g, w).translate(0, h/2, 0);
		return body.union(leg1).union(leg2).union(leg3).union(leg4)
	}
	def setup(): Scene = {
		val scene = Scene(spp=8,showLight=true).
		++(Lighting(x=100, y=900, z=100, size=50)).
		++(Surface.BOX(Vec3(1000,50,3000)).translate(500,0,1500), Gloss(0.75,0.3,0.3,0)).
		++(Surface.BOX(Vec3(1000,2,3000)).translate(500, 1000, 1500), Diffuse(0.85,0.85,0.85)).
		++(Surface.BOX(Vec3(2,1000,3000)).translate(0, 500, 1500), Diffuse(0.85,0.85,0.85)).
		++(Surface.BOX(Vec3(2,1000,3000)).translate(1000, 500, 1500), Diffuse(0.85,0.85,0.85)).
		++(Surface.BOX(Vec3(1000,1000,2)).translate(500, 500, 3000), Diffuse(0.85,0.85,0.85))
		return scene;
	}
}