// case class Terrain(
	
// ) extends Surface with Bounded {
	// def TERRAIN(width: Double, height: Double, depth: Double): BoundedSDF = {
		// return BoundedSDF(
			// (v: Vec3) => {
				// val func = (vec: Vec3) => {vec.y - Noise(1200).layeredNoise(vec.x/200.0, vec.z/200.0, 3)*height}
				// val f = func(v)
				// val grad_x = (f-func(v - Vec3(x=0.001)));
				// val grad_y = (f-func(v - Vec3(y=0.001)));
				// val grad_z = (f-func(v - Vec3(z=0.001)));
				// f/(Vec3(grad_x, grad_y, grad_z).magnitude*1000)
			// },
			// Array(
				// -Vec3(width/2,height/2,depth/2),Vec3(width/2,-height/2,-depth/2),
				// Vec3(width/2,-height/2,depth/2),Vec3(-width/2,-height/2,depth/2),
				// Vec3(-width/2,height/2,-depth/2),Vec3(width/2,height/2,-depth/2),
				// Vec3(width/2,height/2,depth/2),Vec3(-width/2,height/2,depth/2)
			// )
		// )
	// }
// }