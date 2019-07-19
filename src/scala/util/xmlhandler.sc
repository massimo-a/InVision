/*
** Author:  Massimo Angelillo
**
** Handles interactions with XML and XML files,
** along with parsing the XML to generate a scene.
** It contains mutations and side effects
*/

package raytracing.util
import scala.xml.{XML,Elem,NodeSeq};
import scala.io.Source;
import java.io.File
import raytracing.scene.{Scene,Diffuse,Lighting};
import raytracing.geometry.BoundedSDF;

class XMLHandler(filename: String) {
	var scene_xml: Elem = null;
	var scene: Scene = null;
	def fromString() {
		scene_xml = XML.loadFile(new File(filename));
	}
	def getSpheres() {
		val spheres = scene_xml\\"sphere";
		spheres.map(sphere => {
			val radius = (sphere\"@radius").text.toDouble;
			val x = (sphere\"@x").text.toDouble;
			val y = (sphere\"@y").text.toDouble;
			val z = (sphere\"@z").text.toDouble;
			val color = (sphere\\"color")
			scene = scene++(
				BoundedSDF.SPHERE(radius).translate(x, y, z),
				Diffuse(v => {
					Vec3((color\\"@red").text.toDouble, (color\\"@green").text.toDouble, (color\\"@blue").text.toDouble)
				})
			)
		})
	}
	def getBoxes() {
		val boxes = scene_xml\\"box";
		boxes.map(box => {
			val w = (box\"@width").text.toDouble;
			val h = (box\"@height").text.toDouble;
			val d = (box\"@depth").text.toDouble;
			val x = (box\"@x").text.toDouble;
			val y = (box\"@y").text.toDouble;
			val z = (box\"@z").text.toDouble;
			val color = (box\"color")
			scene = scene++(
				BoundedSDF.BOX(w, h, d).translate(x, y, z),
				Diffuse(v => {
					Vec3((color\\"@red").text.toDouble, (color\\"@green").text.toDouble, (color\\"@blue").text.toDouble)
				})
			)
		})
	}
	def getLights() {
		val lights = scene_xml\\"light";
		lights.map(light => {
			val _size = (light\"@size").text.toDouble;
			val _x = (light\"@x").text.toDouble;
			val _y = (light\"@y").text.toDouble;
			val _z = (light\"@z").text.toDouble;
			val color = (light\"color")
			scene = scene++(
				Lighting(
					redEmission = (color\"@red").text.toDouble,
					greenEmission = (color\"@green").text.toDouble,
					blueEmission = (color\"@blue").text.toDouble,
					x = _x, y = _y, z = _z, size = _size
				)
			)
		})
	}
	def getPlanes() {
		val planes = scene_xml\\"plane";
		planes.map(p => {
			val _nx = (p\"@normalx").text.toDouble;
			val _ny = (p\"@normaly").text.toDouble;
			val _nz = (p\"@normalz").text.toDouble;
			val _x = (light\"@x").text.toDouble;
			val _y = (light\"@y").text.toDouble;
			val _z = (light\"@z").text.toDouble;
			val color = (light\"color")
			scene = scene++(
				Plane(Vec3(_nx, _ny, _nz).normalize, Vec3(_x, _y, _z)),
				Diffuse(v => {
					Vec3((color\\"@red").text.toDouble, (color\\"@green").text.toDouble, (color\\"@blue").text.toDouble)
				})
			)
		})
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
			val _spp = (_scene(0)\"@spp").text.toInt
			val _width = (_scene(0)\"@width").text.toInt
			val _height = (_scene(0)\"@height").text.toInt
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