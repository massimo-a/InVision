// trait VectorSpace[A] {
	// def unary_-(): VectorSpace;
	// def +(v: VectorSpace): VectorSpace;
	// def -(v: VectorSpace): VectorSpace;
	// def scale(a: A): VectorSpace;
	// def dot(v: VectorSpace): A;
	// def *(v: VectorSpace): VectorSpace;
// }
// case class Vec3(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0) extends VectorSpace[Double] {
	// def unary_-(): Vec3 = {
		// return this(-x, -y, -z);
	// }
	// def +(a: Vec3): Vec3 = {
		// return this(x+a.x, y+a.y, z+a.z);
	// }
	// def -(a: Vec3): Vec3 = {
		// return this(x-a.x, y-a.y, z-a.z);
	// }
	// def scale(b: Double): Vec3 = {
		// return this(b*x, b*y, b*z);
	// }
	// def dot(a: Vec3): Double = {
		// return a.x*x + a.y*y + a.z*z;
	// }
	// def *(a: Vec3): Vec3 = {
		// return Vec3(y*v.z-z*v.y, z*v.x-x*v.z, x*v.y-y*v.x);
	// }
// }
// case class Quat(real: Double, vect: Vec3) extends VectorSpace[Double] {
	// def unary-(): Quat = {
		// return this(-real, -vect);
	// }
	// def conj(): Quat = {
		// return this(real, -vect);
	// }
	// def +(q: Quat): Quat = {
		// return this(real+q.real, vect+q.vect);
	// }
	// def -(q: Quat): Quat = {
		// return this(real-q.real, vect-q.vect);
	// }
	// def scale(b: Double): Quat = {
		// return this(b*real, b*vect);
	// }
	// def dot(q: Quat): Double = {
		// return real*q.real + vect.dot(q.vect);
	// }
	// def *(q: Quat): Quat = {
		// return Quat(real*q.real - vect.dot(q.vect), real*q.vect + q.real*vect + vect*q.vect);
	// }
// }