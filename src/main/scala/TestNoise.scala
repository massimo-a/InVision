/*
** Author:  Massimo Angelillo
*/

import java.awt.image.BufferedImage
import java.io.File

import invision.util
import invision.util._
import javax.imageio.ImageIO

object TestNoise {
	def testWorley() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB)

		val blocks = 10
		val pts = new Array[Vec3](blocks*blocks)
		for(i <- 0 until blocks) {
			for(j <- 0 until blocks) {
				pts(i + j*blocks) = Vec3((i+Math.random)/blocks, (j+Math.random)/blocks)
			}
		}
		
		val n = Array(Worley(pts), Worley(pts, Distance.chebyshev), Worley(pts, Distance.manhattan))
		for(k <- n.indices) {
			for(i <- 0 until 1000) {
				for(j <- 0 until 1000) {
					val pn = Noise.get(n(k), i/1000.0, j/1000.0)*1.4142*5

					val col = Vec3(1,1,1).lerp(Vec3(), pn) *255

					val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt

					im.setRGB(i, j, rgb)
				}
			}
			ImageIO.write(im, "png", new File("worley_test_" + k + ".png"))
		}
	}
	
	def testValue() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB)
		val n = Value(14589124)
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = Noise.get(n, i/200.0, j/200.0)
				val col = Vec3(1,1,1).lerp(Vec3(), pn) *255
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File("value_test.png"))
	}
	
	def testValueFractal() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB)
		val n = Value(14589124)
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = Noise.fractalize(n, 10, i/200.0, j/200.0)
				val col = Vec3(1,1,1).lerp(Vec3(), pn) *255
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File("fractal_value_test.png"))
	}
	
	def testValueFractal2() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB)
		val n = Value(14589124)
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = Noise.fractalizeWith(x => Math.abs(x), n, 10, i/200.0, j/200.0)
				val col = Vec3(1,1,1).lerp(Vec3(), pn) *255
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File("fractal_value_test_2.png"))
	}
	
	def main(args: Array[String]): Unit = {
		println("started worley test...")
		testWorley()
		println("completed worley test")
		println("started value test...")
		testValue()
		println("completed value test")
		println("started fractal value test...")
		testValueFractal()
		testValueFractal2()
		println("completed fractal value test")
		println("completed all tests")
	}
}