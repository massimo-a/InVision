/*
** Author:  Massimo Angelillo
*/

package raytracing.scene
import raytracing.{geometry,scene,util},geometry._,scene._,util._
import scala.math.{Pi,sin,cos,abs}

object Presets {
	def Test(n: Int, w: Int, h: Int): Scene = {
		val no = Value("helloworld")
		val terr = SurfaceMarcher.Sphere(300)
			.translate(500, 500, 600)
			.distort(v => Math.sin(Noise.get(no, v.x/150.0, v.y/150.0)*Math.PI)*35.0, 5.0)
		Scene(spp=n, width=w, height=h)++(
			BallLight(r=30,x=500,y=900,z=200)
		)++(
			terr,
			Diffuse(),
			Vec3(1, 0, 0)
		)
	}
}

