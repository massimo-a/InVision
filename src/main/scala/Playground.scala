/**
 * @author Massimo Angelillo
 *
 * A special thank you to Kevin Sangurima for assistance in
 * solving some algorithm issues, naming it and testing it out!
 */

import invision.geometry.Crystal
import invision.scene.material.Diffuse
import invision.scene.renderable.BallLight
import invision.scene.{Camera, PathTracer, World}
import invision.util.Vec3

import java.awt.image.BufferedImage
import java.io.{File, FileNotFoundException, FileWriter}
import java.text.SimpleDateFormat
import java.util.Calendar
import javax.imageio.ImageIO

object Playground {
  private val width = 1280
  private val height = 960
  private val spp = 3
  private val pictureSaveLocation = "pictures\\test pictures\\"
  private val pictureName = "test"

  private def logData(t: Double) {
    val format = new SimpleDateFormat("d_M_y")
    val correctPath = if (pictureSaveLocation.endsWith("\\")) "" else "\\"
    val logPath = s"$pictureSaveLocation${correctPath}LOG_${format.format(Calendar.getInstance().getTime)}.log"
    val log = new File(logPath)
    try {
      if (!log.exists) {
        log.createNewFile()
      }
      val pw = new FileWriter(log, true)
      pw.write(s"file name - ${pictureName}_$spp \r\n")
      pw.write(s"run time in seconds - $t \r\n")
      pw.write(s"anti-aliasing - $spp \r\n")
      pw.write(s"screen size - ($height, $width) \r\n")
      try {
        pw.write(s"primary rays shot per second - ${height * width * spp * spp / t} \r\n")
      } catch {
        case _: ArithmeticException =>
          pw.write("primary rays shot per second - TOO MANY \r\n")
      }
      pw.close()
    } catch {
      case _: FileNotFoundException => println("Could not save picture, file path not found")
    }
  }

  private def saveImage(im: BufferedImage) {
    try {
      val correctPath = if (pictureSaveLocation.endsWith("\\") || pictureName.head == '\\') "" else "\\"
      ImageIO.write(im, "png", new File(s"$pictureSaveLocation$correctPath${pictureName}_$spp.png"))
      println(s"Completed: $pictureSaveLocation$correctPath${pictureName}_$spp")
    } catch {
      case _: FileNotFoundException => println("Could not save picture, file path not found")
    }
  }

  def main(args: Array[String]): Unit = {
    val world: World = World() ++
      (Crystal(300, 500, Vec3(-200, 500, 700), Vector(Vec3(1, 0, -1).normalize(), Vec3(0, 1), Vec3(1, 0, 1).normalize())), Diffuse(1), Vec3(0.9, 0.9, 0.2)) ++
      BallLight(1, 600, 1000, -100)

    val renderer = PathTracer(world, camera = Camera(width, height, Math.PI/8.0), width=width, height=height, spp=spp)

    val start = System.currentTimeMillis()
    println("Begun Rendering")
    println(s"Start Time: ${Calendar.getInstance().getTime}")
    val arr: BufferedImage = renderer.render()
    val end = System.currentTimeMillis()
    saveImage(arr)
    println(s"Completion Time: ${(end - start)/1000.0} seconds")
    logData((end - start)/1000.0)
  }
}