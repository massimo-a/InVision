package invision.geometry

case class Face(vertexIndexOne: Int, vertexIndexTwo: Int, vertexIndexThree: Int)

case object Face {
  def from(faces: Seq[Int]): Vector[Face] = {
    faces
      .grouped(3)
      .map(x => Face(x.head, x(1), x(2)))
      .toVector
  }
}
