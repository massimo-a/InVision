/*
** Author:  Massimo Angelillo
*/

package raytracing.util

final case class RaList[A](
	elem: A,
	left: RaList[A]=null,
	right: RaList[A]=null,
	length: Int = 1
) extends {
	private def insert(l: RaList[A], a: A, targetTier: Int): RaList[A] = {
		val m = (l.length+1) % (Math.pow(2, targetTier))
		if(m < Math.pow(2, targetTier - 1)) {
			if(l.left == null) {
				return RaList[A](l.elem, RaList(a), l.right, l.length+1)
			} else {
				return RaList[A](l.elem, insert(l.left, a, targetTier-1), l.right, l.length+1)
			}
		} else {
			if(l.right == null) {
				return RaList[A](l.elem, l.left, RaList(a), length+1)
			} else {
				return RaList[A](l.elem, l.left, insert(l.right, a, targetTier-1), l.length+1)
			}
		}
	}
	
	def insert(a: A): RaList[A] = {
		insert(this, a, (Math.floor(Math.log(length+1)/Math.log(2))).toInt)
	}
	
	private def get(l: RaList[A], i: Int, targetTier: Int): A = {
		if(targetTier == 0) return l.elem
		val m = i % Math.pow(2, targetTier)
		if(m < Math.pow(2, targetTier-1)) {
			return get(l.left, i, targetTier-1)
		} else {
			return get(l.right, i, targetTier-1)
		}
	}
	
	def get(i: Int): A = {
		get(this, i+1, Math.floor(Math.log(i+1)/Math.log(2)).toInt)
	}
	
	def indexedTraversal[B](f: (A, Int) => B, accu: RaList[B], index: Int): RaList[B] = {
		if(index == 0) {
			return accu
		}
		return indexedTraversal(f, accu.insert(f(get(index), index)), index-1)
	}
	
	def indexedTraversal[B](f: (A, Int) => B): RaList[B] = {
		indexedTraversal(f, null, length)
	}
	
	def fold[B](f: (B, A) => B, accu: B, index: Int): B = {
		if(index == 0) {
			return f(accu, get(0))
		}
		return fold(f, f(accu, get(index)), index-1)
	}
	
	def fold[B](init: B, f: (B, A) => B): B = {
		fold(f, init, length-1)
	}
}