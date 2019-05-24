package raytracing.util

case class Noise(interval: Double) {
	private def lerp(a: Double, b: Double, c: Double): Double = {
		return a*(1-c) + b*c
	}
	def noise(x: Double, y: Double): Double = {
		val p = Vec3(x%1, y%1)
		val p1 = Vec3(0,0)
		val p2 = Vec3(0,1)
		val p3 = Vec3(1,1)
		val p4 = Vec3(1,0)
		
		val grad_p1 = Vec3(Math.random()*2-1, Math.random()*2-1);
		val grad_p2 = Vec3(Math.random()*2-1, Math.random()*2-1);
		val grad_p3 = Vec3(Math.random()*2-1, Math.random()*2-1);
		val grad_p4 = Vec3(Math.random()*2-1, Math.random()*2-1);
		
		val dif_p1 = p-p1
		val dif_p2 = p-p2
		val dif_p3 = p-p3
		val dif_p4 = p-p4
		
		val dot_p1 = dif_p1*grad_p1
		val dot_p2 = dif_p2*grad_p2
		val dot_p3 = dif_p3*grad_p3
		val dot_p4 = dif_p4*grad_p4
		
		val lerp1 = lerp(dot_p1, dot_p2, x%1)
		val lerp2 = lerp(dot_p3, dot_p4, x%1)
		
		return lerp(lerp1, lerp2, y%1)
	}
}