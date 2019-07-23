/*
** Author:  Massimo Angelillo
**
** Handles interactions with XML and XML files,
** along with parsing the XML to generate a scene.
** It contains mutations and side effects
*/

package raytracing.util
import scala.xml.{XML,Elem,Node,NodeSeq};
import scala.io.Source;
import java.io.File
import raytracing.scene.{Scene,Diffuse,Lighting};
import raytracing.geometry.{BoundedSDF,Plane};

class XMLHandler(filename: String) {
	var scene_xml: Elem = null;
	var scene: Scene = null;
	def fromString() {
		scene_xml = XML.loadFile(new File(filename));
	}
	def getAttrOrElse(parent:NodeSeq, attr: String, default: Double): Double = {
		val a = parent\("@" + attr)
		if(a.text.equals("")) {
			return default
		} else {
			return a.text.toDouble
		}
	}
	def getSpheres() {
		val spheres = scene_xml\\"sphere";
		spheres.map(sphere => {
			val radius = getAttrOrElse(sphere, "radius", 100);
			val x = getAttrOrElse(sphere, "x", 0);
			val y = getAttrOrElse(sphere, "y", 0);
			val z = getAttrOrElse(sphere, "z", 0);
			val color = (sphere\\"color")
			scene = scene++(
				BoundedSDF.SPHERE(radius).translate(x, y, z),
				Diffuse(v => {
					Vec3(getAttrOrElse(color,"red",1.0),getAttrOrElse(color,"green",1.0),getAttrOrElse(color,"blue",1.0))
				})
			)
		})
	}
	def getBoxes() {
		val boxes = scene_xml\\"box";
		boxes.map(box => {
			val w = getAttrOrElse(box, "width", 100);
			val h = getAttrOrElse(box, "height", 100);
			val d = getAttrOrElse(box, "depth", 100);
			val x = getAttrOrElse(box, "x", 0);
			val y = getAttrOrElse(box, "y", 0);
			val z = getAttrOrElse(box, "z", 0);
			val color = (box\"color")
			scene = scene++(
				BoundedSDF.BOX(w, h, d).translate(x, y, z),
				Diffuse(v => {
					Vec3(getAttrOrElse(color,"red",1.0),getAttrOrElse(color,"green",1.0),getAttrOrElse(color,"blue",1.0))
				})
			)
		})
	}
	def getLights() {
		val lights = scene_xml\\"light";
		lights.map(light => {
			val _size = getAttrOrElse(light, "size", 20);
			val _x = getAttrOrElse(light, "x", 0);
			val _y = getAttrOrElse(light, "y", 0);
			val _z = getAttrOrElse(light, "z", 0);
			val color = (light\"color")
			scene = scene++(
				Lighting(
					redEmission = getAttrOrElse(color,"red",1.0),
					greenEmission = getAttrOrElse(color,"green",1.0),
					blueEmission = getAttrOrElse(color,"blue",1.0),
					x = _x, y = _y, z = _z, size = _size
				)
			)
		})
	}
	def getPlanes() {
		val planes = scene_xml\\"plane";
		planes.map(p => {
			val _nx = getAttrOrElse(p, "normalx", 0);
			val _ny = getAttrOrElse(p, "normaly", 0);
			val _nz = getAttrOrElse(p, "normalz", 0);
			val _x = getAttrOrElse(p, "x", 0);
			val _y = getAttrOrElse(p, "y", 0);
			val _z = getAttrOrElse(p, "z", 0);
			val color = (p\"color")
			scene = scene++(
				Plane(Vec3(_nx, _ny, _nz).normalize, Vec3(_x, _y, _z)),
				Diffuse(v => {
					Vec3(getAttrOrElse(color,"red",1.0),getAttrOrElse(color,"green",1.0),getAttrOrElse(color,"blue",1.0))
				})
			)
		})
	}
	def getMaterial(e: NodeSeq) {
		
	}
	def getAll() {
		getSpheres();
		getBoxes();
		getLights();
		getPlanes();
	}
	def setupScene() {
		val _scene = scene_xml\\"scene";
		if(!_scene.text.equals("")) {
			val _spp = getAttrOrElse(_scene(0), "spp", 1).toInt;
			val _width = getAttrOrElse(_scene(0), "width", 500).toInt
			val _height = getAttrOrElse(_scene(0), "height", 500).toInt
			scene = Scene(spp = _spp, width = _width, height = _height)
		}
	}
	def load(): Scene = {
		fromString();
		setupScene();
		getAll();
		return scene;
	}
}