// import scala.xml.XML

// case class XMLHandler(filename: String) {
	// var scene = Scene(spp=5);
	// def fromString(str: String): Elem {
		// return XML.loadString(str);
	// }
	// def getSpheres(x: Elem): NodeSeq = {
		// val spheres = x\\"sphere";
		// spheres.forEach(sphere => {
			// scene = scene++(
				// BoundedSDF.SPHERE((sphere\\"@radius").text.toDouble).translate((sphere\\"@x").text.toDouble, (sphere\\"@y").text.toDouble, (sphere\\"@z").text.toDouble),
				// Diffuse(v => {
					// Vec3((sphere\\"@red").text.toDouble, (sphere\\"@green").text.toDouble, (sphere\\"@blue").text.toDouble)
				// })
			// )
		// })
	// }
// }