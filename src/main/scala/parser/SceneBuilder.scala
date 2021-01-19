package parser

import invision.geometry.intersectable.{Plane, Square, SurfaceMarcher, Triangle}
import invision.scene.World
import invision.scene.material.{Diffuse, Gloss, Material, Transparent}
import invision.scene.renderable.{BallLight, Light, SceneObject}
import invision.util.Vec3
import parser.tokenizer.Tokenizer

import scala.io.Source
import scala.math.Pi

final case class SceneBuilder() {
  @scala.annotation.tailrec
  def buildWorld(parsed: List[(String, Map[String, List[String]])], world: World = World()): World = {
    if(parsed.isEmpty) {
      world
    } else if(parsed.head._1 == "LIGHT") {
      buildWorld(parsed.drop(1), world ++ buildLight(parsed.head._2))
    } else {
      buildWorld(parsed.drop(1), world ++ build(parsed.head))
    }
  }

  def build(shape: (String, Map[String, List[String]])): SceneObject = {
    shape._1 match {
      case "SPHERE" => buildSphere(shape._2)
      case "PLANE" => buildPlane(shape._2)
      case "TRIANGLE" => buildTriangle(shape._2)
      case "QUAD" => buildQuad(shape._2)
      case "BOX" => buildBox(shape._2)
      case "TORUS" => buildTorus(shape._2)
      case "CYLINDER" => buildCylinder(shape._2)
      case _ => throw new Exception(s"Unknown scene object declaration ${shape._1}")
    }
  }

  def load(file: String): World = {
    val bufferedSource = Source.fromFile(file)
    var t = ""
    val parser = Parser()
    val tok = Tokenizer()
    for (line <- bufferedSource.getLines) {
      if(!line.startsWith("//")) {
        t += line
      }
    }
    bufferedSource.close
    buildWorld(parser.parse(tok.tokenize(t)))
  }

  /**
   * Build methods
   */
  private def buildBox(details: Map[String, List[String]]): SceneObject = {
    val pos = buildVector(details, "POSITION")
    val size = buildVector(details, "SIZE")
    val rotate = buildVector(details, "ROTATE").map(x => x*2*Pi/360.0)
    val color = buildVector(details, "COLOR")
    val mat = buildMaterial(details)

    SceneObject(SurfaceMarcher.Box(size).rotate(rotate).translate(pos), mat, color)
  }

  private def buildCylinder(details: Map[String, List[String]]): SceneObject = {
    val pos = buildVector(details, "POSITION")
    val radius = if (details.contains("RADIUS")) {
      details("RADIUS").head.toDouble
    } else {
      throw new Exception()
    }
    val height = if (details.contains("HEIGHT")) {
      details("HEIGHT").head.toDouble
    } else {
      throw new Exception()
    }
    val rotate = buildVector(details, "ROTATE").map(x => x*2*Pi/360.0)
    val color = buildVector(details, "COLOR")
    val mat = buildMaterial(details)

    SceneObject(SurfaceMarcher.Cylinder(radius, height).rotate(rotate).translate(pos), mat, color)
  }

  private def buildPlane(values: Map[String, List[String]]): SceneObject = {
    val normal = buildVector(values, "NORMAL")
    val pos = buildVector(values, "POSITION")
    val col = buildVector(values, "COLOR")
    val mat = buildMaterial(values)

    SceneObject(Plane(normal=normal,point=pos), mat, col)
  }

  private def buildQuad(values: Map[String, List[String]]): SceneObject = {
    val v1 = buildVector(values, "VERTEXONE")
    val v2 = buildVector(values, "VERTEXTWO")
    val v3 = buildVector(values, "VERTEXTHREE")
    val v4 = buildVector(values, "VERTEXFOUR")
    val color = buildVector(values, "COLOR")
    val mat = buildMaterial(values)

    SceneObject(Square(v1, v2, v3, v4), mat, color)
  }

  private def buildSphere(values: Map[String, List[String]]): SceneObject = {
    val pos = buildVector(values, "POSITION")

    val radius = if (values.contains("RADIUS")) {
      values("RADIUS").head.toDouble
    } else {
      throw new Exception()
    }

    val color = buildVector(values, "COLOR")
    val mat = buildMaterial(values)

    SceneObject(SurfaceMarcher.Sphere(radius).translate(pos), mat, color)
  }

  private def buildTorus(details: Map[String, List[String]]): SceneObject = {
    val pos = buildVector(details, "POSITION")
    val centerRadius = if (details.contains("CENTERRADIUS")) {
      details("CENTERRADIUS").head.toDouble
    } else {
      throw new Exception()
    }
    val tubeRadius = if (details.contains("TUBERADIUS")) {
      details("TUBERADIUS").head.toDouble
    } else {
      throw new Exception()
    }
    val rotate = buildVector(details, "ROTATE").map(x => x*2*Pi/360.0)
    val color = buildVector(details, "COLOR")
    val mat = buildMaterial(details)

    SceneObject(SurfaceMarcher.Torus(centerRadius, tubeRadius).rotate(rotate).translate(pos), mat, color)
  }

  private def buildTriangle(values: Map[String, List[String]]): SceneObject = {
    val v1 = buildVector(values, "VERTEXONE")
    val v2 = buildVector(values, "VERTEXTWO")
    val v3 = buildVector(values, "VERTEXTHREE")
    val color = buildVector(values, "COLOR")
    val mat = buildMaterial(values)

    SceneObject(Triangle(v1, v2, v3), mat, color)
  }

  private def buildLight(details: Map[String, List[String]]): Light = {
    val pos = buildVector(details, "POSITION")

    val radius = if (details.contains("RADIUS")) {
      details("RADIUS").head.toDouble
    } else {
      throw new Exception()
    }

    val col = buildVector(details, "COLOR")

    BallLight(size=radius,x=pos.x,y=pos.y,z=pos.z,color=col)
  }

  private def buildVector(details: Map[String, List[String]], key: String, default: Vec3 = null): Vec3 = {
    if(details.contains(key)) {
      Vec3(details(key).head.toDouble, details(key)(1).toDouble, details(key)(2).toDouble)
    } else {
      if(default == null) {
        throw new Exception(s"Required element $key not found")
      } else {
        default
      }
    }
  }

  private def buildMaterial(details: Map[String, List[String]]): Material = {
    if(details.contains("DIFFUSE")) {
      Diffuse(details("DIFFUSE").head.toDouble)
    } else if(details.contains("GLOSS")) {
      Gloss(details("GLOSS").head.toDouble, details("GLOSS")(1).toDouble)
    } else if(details.contains("TRANSPARENT")) {
      Transparent(details("TRANSPARENT").head.toDouble, details("TRANSPARENT")(1).toDouble)
    } else {
      Diffuse()
    }
  }
}
