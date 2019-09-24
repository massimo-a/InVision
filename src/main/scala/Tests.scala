/*
** Author:  Massimo Angelillo
*/

import javax.imageio.ImageIO;
import java.io.File
import java.awt.image.BufferedImage
import annotation.tailrec;
import raytracing.{geometry,scene,util},geometry._,scene._,util._;

object TestNoise {
	def testWorley() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		
		val blocks = 10;
		val pts = new Array[Vec3](blocks*blocks);
		for(i <- 0 until blocks) {
			for(j <- 0 until blocks) {
				pts(i + j*blocks) = Vec3((i+Math.random)/blocks, (j+Math.random)/blocks)
			}
		}
		
		val n = Array(Worley(pts), Worley(pts, Distance.chebyshev), Worley(pts, Distance.manhattan))
		for(k <- 0 until n.length) {
			for(i <- 0 until 1000) {
				for(j <- 0 until 1000) {
					val pn = Noise.get(n(k), i/1000.0, j/1000.0)*1.4142*5;
					
					val col = (Vec3(1,1,1).lerp(Vec3(0,0,0), pn))*255;
					
					val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
					
					im.setRGB(i, j, rgb)
				}
			}
			ImageIO.write(im, "png", new File("worley_test_" + k + ".png"));
		}
	}
	
	def testValue() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		val n = Value(14589124)
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = Noise.get(n, i/200.0, j/200.0);
				val col = (Vec3(1,1,1).lerp(Vec3(0,0,0), pn))*255;
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File("value_test.png"));
	}
	
	def testValueFractal() {
		val im = new BufferedImage(1000,1000,BufferedImage.TYPE_INT_RGB);
		val n = Value(14589124)
		for(i <- 0 until 1000) {
			for(j <- 0 until 1000) {
				val pn = Noise.fractalize(n, 10, i/200.0, j/200.0);
				val col = (Vec3(1,1,1).lerp(Vec3(0,0,0), pn))*255;
				val rgb = col.x.toInt*256*256 + col.y.toInt*256 + col.z.toInt;
				im.setRGB(i, j, rgb)
			}
		}
		ImageIO.write(im, "png", new File("fractal_value_test.png"));
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
		println("completed fractal value test")
		println("completed all tests")
	}
}

object TestRaList {
	var list = RaList[Double](4.2)
	def main(args: Array[String]): Unit = {
		for(k <- 1E6 until 1E8 by 1E6) {
			for(i <- 0 until k.toInt) {
				val r = Math.random()
				list = RaList.add(list, r)
			}
			val a = RaList.get(list, list.length-1)
		}
	}
}

object TestTree {
	def main(args: Array[String]) {
		var numPts = 1000000
		var pointsIn =  Array.tabulate(numPts)(x => Vec3(Math.random*500))
		
		var tree: Tree = BinaryTree(pointsIn(0));
		var pass = true;
		
		for(i <- 1 until numPts) {
			tree = tree.add(pointsIn(i))
		}
		for(i <- 0 until numPts) {
			if(!tree.contains(pointsIn(i))) {
				pass = false
			}
		}
		println("BinaryTree : " + {if(pass) "PASS" else "FAIL"})
		
		tree = QuadTree(pointsIn(0));
		pass = true;
		
		for(i <- 1 until numPts) {
			tree = tree.add(pointsIn(i))
		}
		for(i <- 0 until numPts) {
			if(!tree.contains(pointsIn(i))) {
				pass = false
			}
		}
		println("QuadTree : " + {if(pass) "PASS" else "FAIL"})
		
		tree = QuadTree(pointsIn(0));
		pass = true;
		
		for(i <- 1 until 1000000) {
			tree = tree.add(pointsIn(i))
		}
		for(i <- 0 until 1000000) {
			if(!tree.contains(pointsIn(i))) {
				pass = false
			}
		}
		println("OctTree : " + {if(pass) "PASS" else "FAIL"})
	}
}