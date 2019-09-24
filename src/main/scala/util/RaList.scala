/*
** Author:  Massimo Angelillo
*/

package raytracing.util

sealed trait List[+A]
case object Nil extends List[Nothing]
final case class RaList[+A](
	elem: A,
	left: List[A]=Nil,
	right: List[A]=Nil,
	length: Int = 1
) extends List[A]

object RaList {
	private def add[A](list: List[A], a: A, targetTier: Int): RaList[A] = {
		list match {
			case Nil => RaList(a)
			case RaList(e,l,r,len) => {
				val m = (len+1) % (Math.pow(2, targetTier))
				if(m < Math.pow(2, targetTier - 1)) {
					RaList[A](e, add(l, a, targetTier-1), r, len+1)
				} else {
					RaList[A](e, l, add(r, a, targetTier-1), len+1)
				}
			}
		}
	}
	
	def add[A](list: List[A], a: A): RaList[A] = {
		list match {
			case Nil => RaList(a)
			case RaList(e,l,r,len) => {
				add(list, a, (Math.floor(Math.log(len+1)/Math.log(2))).toInt)
			}
		}
	}
	
	private def get[A](list: List[A], i: Int, targetTier: Int): A = {
		list match {
			case Nil => throw new Exception("Index out of bounds")
			case RaList(e, l, r, len) => {
				if(targetTier == 0) return e
				val m = i % Math.pow(2, targetTier)
				if(m < Math.pow(2, targetTier-1)) {
					return get(l, i, targetTier-1)
				} else {
					return get(r, i, targetTier-1)
				}
			}
		}
	}
	
	def get[A](list: List[A], i: Int): A = {
		list match {
			case Nil => throw new Exception("Index out of bounds")
			case RaList(e, l, r, len) => {
				get(list, i+1, Math.floor(Math.log(i)/Math.log(2)).toInt)
			}
		}
	}
}