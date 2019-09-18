package raytracing.util

trait Tree {
	def add(p: Vec3): Tree;
	def contains(p: Vec3): Boolean;
}

// Leaf Objects
case object BinaryLeaf extends Tree {
	def add(pt: Vec3): BinaryTree = {BinaryTree(pt)}
	def contains(pt: Vec3): Boolean = {false}
}
case object QuadLeaf extends Tree {
	def add(pt: Vec3): QuadTree = {QuadTree(pt)}
	def contains(pt: Vec3): Boolean = {false}
}
case object OctLeaf extends Tree {
	def add(pt: Vec3): OctTree = {OctTree(pt)}
	def contains(pt: Vec3): Boolean = {false}
}

// Tree Classes
case class BinaryTree(
	point:Vec3=Vec3(),
	left:Tree=BinaryLeaf,
	right:Tree=BinaryLeaf
) extends Tree {
	def add(pt: Vec3): BinaryTree = {
		if(pt.x < point.x) BinaryTree(point, left.add(pt), right)
		else BinaryTree(point, left, right.add(pt))
	}
	def contains(pt: Vec3): Boolean = {
		if(~(pt - point) < 1) true
		else if(pt.x < point.x) left.contains(pt)
		else right.contains(pt)
	}
}

case class QuadTree(
	point:Vec3=Vec3(),
	q1:Tree=QuadLeaf,
	q2:Tree=QuadLeaf,
	q3:Tree=QuadLeaf,
	q4:Tree=QuadLeaf
) extends Tree {
	def add(pt: Vec3): QuadTree = {
		if(pt.x >= point.x && pt.y >= point.y) QuadTree(point, q1.add(pt), q2, q3, q4)
		else if(pt.x < point.x && pt.y >= point.y) QuadTree(point, q1, q2.add(pt), q3, q4)
		else if(pt.x < point.x && pt.y < point.y) QuadTree(point, q1, q2, q3.add(pt), q4)
		else QuadTree(point, q1, q2, q3, q4.add(pt))
	}
	def contains(pt: Vec3): Boolean = {
		if(~(pt - point) < 1) true
		else if(pt.x >= point.x && pt.y >= point.y) q1.contains(pt)
		else if(pt.x < point.x && pt.y >= point.y) q2.contains(pt)
		else if(pt.x < point.x && pt.y < point.y) q3.contains(pt)
		else q4.contains(pt)
	}
}

case class OctTree(
	point:Vec3=Vec3(),
	q1:Tree=OctLeaf,
	q2:Tree=OctLeaf,
	q3:Tree=OctLeaf,
	q4:Tree=OctLeaf,
	q5:Tree=OctLeaf,
	q6:Tree=OctLeaf,
	q7:Tree=OctLeaf,
	q8:Tree=OctLeaf
) extends Tree {
	def add(pt: Vec3): OctTree = {
		if(pt.x >= point.x && pt.y >= point.y && pt.z >= point.z) OctTree(point, q1.add(pt), q2, q3, q4, q5, q6, q7, q8)
		else if(pt.x < point.x && pt.y >= point.y && pt.z >= point.z) OctTree(point, q1, q2.add(pt), q3, q4, q5, q6, q7, q8)
		else if(pt.x < point.x && pt.y < point.y && pt.z >= point.z) OctTree(point, q1, q2, q3.add(pt), q4, q5, q6, q7, q8)
		else if(pt.x >= point.x && pt.y < point.y && pt.z >= point.z) OctTree(point, q1, q2, q3, q4.add(pt), q5, q6, q7, q8)
		else if(pt.x >= point.x && pt.y >= point.y && pt.z <= point.z) OctTree(point, q1, q2, q3, q4, q5.add(pt), q6, q7, q8)
		else if(pt.x < point.x && pt.y >= point.y && pt.z <= point.z) OctTree(point, q1, q2, q3, q4, q5, q6.add(pt), q7, q8)
		else if(pt.x < point.x && pt.y < point.y && pt.z <= point.z) OctTree(point, q1, q2, q3, q4, q5, q6, q7.add(pt), q8)
		else OctTree(point, q1, q2, q3, q4, q5, q6, q7, q8.add(pt))
	}
	def contains(pt: Vec3): Boolean = {
		if(~(pt - point) < 1) true
		else if(pt.x >= point.x && pt.y >= point.y && pt.z >= point.z) q1.contains(pt)
		else if(pt.x < point.x && pt.y >= point.y && pt.z >= point.z) q2.contains(pt)
		else if(pt.x < point.x && pt.y < point.y && pt.z >= point.z) q3.contains(pt)
		else if(pt.x >= point.x && pt.y < point.y && pt.z >= point.z) q4.contains(pt)
		else if(pt.x >= point.x && pt.y >= point.y && pt.z <= point.z) q5.contains(pt)
		else if(pt.x < point.x && pt.y >= point.y && pt.z <= point.z) q6.contains(pt)
		else if(pt.x < point.x && pt.y < point.y && pt.z <= point.z) q7.contains(pt)
		else q8.contains(pt)
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