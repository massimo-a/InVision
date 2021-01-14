package parser

import invision.geometry.intersectable.{Plane, Square, SurfaceMarcher, Triangle}
import invision.scene.World
import invision.scene.material.{Diffuse, Gloss, Material, Transparent}
import invision.scene.renderable.{BallLight, Light, SceneObject}
import invision.util.Vec3

import scala.io.Source

final case class WorldFactory() {
  private def makeSphere(details: Map[String, List[String]]): SceneObject = {
    val pos = makeVector(details, "POSITION")

    val radius = if (details.contains("RADIUS")) {
      details("RADIUS").head.toDouble
    } else {
      throw new Exception()
    }

    val color = makeVector(details, "COLOR")
    val mat = getMaterial(details)

    SceneObject(SurfaceMarcher.Sphere(radius).translate(pos), mat, color)
  }

  private def makeLight(details: Map[String, List[String]]): Light = {
    val pos = makeVector(details, "POSITION")

    val radius = if (details.contains("RADIUS")) {
      details("RADIUS").head.toDouble
    } else {
      throw new Exception()
    }

    val col = makeVector(details, "COLOR")

    BallLight(size=radius,x=pos.x,y=pos.y,z=pos.z,color=col)
  }

  private def makePlane(details: Map[String, List[String]]): SceneObject = {
    val normal = makeVector(details, "NORMAL")
    val pos = makeVector(details, "POSITION")
    val col = makeVector(details, "COLOR")
    val mat = getMaterial(details)

    SceneObject(Plane(normal=normal,point=pos), mat, col)
  }

  private def makeVector(details: Map[String, List[String]], key: String): Vec3 = {
    if(details.contains(key)) {
      Vec3(details(key).head.toDouble, details(key)(1).toDouble, details(key)(2).toDouble)
    } else {
      throw new Exception(s"Required element $key not found")
    }
  }

  private def makeTriangle(details: Map[String, List[String]]): SceneObject = {
    val v1 = makeVector(details, "VERTEXONE")
    val v2 = makeVector(details, "VERTEXTWO")
    val v3 = makeVector(details, "VERTEXTHREE")
    val color = makeVector(details, "COLOR")
    val mat = getMaterial(details)

    SceneObject(Triangle(v1, v2, v3), mat, color)
  }

  private def makeQuad(details: Map[String, List[String]]): SceneObject = {
    val v1 = makeVector(details, "VERTEXONE")
    val v2 = makeVector(details, "VERTEXTWO")
    val v3 = makeVector(details, "VERTEXTHREE")
    val v4 = makeVector(details, "VERTEXFOUR")
    val color = makeVector(details, "COLOR")
    val mat = getMaterial(details)

    SceneObject(Square(v1, v2, v3, v4), mat, color)
  }

  private def getMaterial(details: Map[String, List[String]]): Material = {
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

  @scala.annotation.tailrec
  def makeWorld(scene: List[(String, Map[String, List[String]])], world: World = World()): World = {
    if(scene.isEmpty) {
      return world
    }

    scene.head._1 match {
      case "SPHERE" =>
        makeWorld(scene.drop(1), world ++ makeSphere(scene.head._2))
      case "PLANE" =>
        makeWorld(scene.drop(1), world ++ makePlane(scene.head._2))
      case "LIGHT" =>
        makeWorld(scene.drop(1), world ++ makeLight(scene.head._2))
      case "TRIANGLE" =>
        makeWorld(scene.drop(1), world ++ makeTriangle(scene.head._2))
      case "QUAD" =>
        makeWorld(scene.drop(1), world ++ makeQuad(scene.head._2))
      case _ =>
        throw new Exception(s"Unknown scene object declaration ${scene.head._1}")
    }
  }

  def load(file: String): World = {
    val bufferedSource = Source.fromFile(file)
    var t = ""
    val parser = Parser()
    for (line <- bufferedSource.getLines) {
      if(!line.startsWith("//")) {
        t += line
      }
    }
    bufferedSource.close
    makeWorld(parser.parse(Tokenizer.tokenize(t)))
  }
}