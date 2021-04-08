class Point(val x: Int, val y: Int) {
  override def equals(other: Any) = other match {
    case that: Point => this.x == that.x && this.y == that.y
    case _ => false
  }
  // XXX: oops - didn't override hashCode.
  //override def hashCode() = 41 * (41 + x) + y
}

import scala.collection.mutable._

val p1, p2 = new Point(1,2)

val ps = HashSet(p1)

println(ps contains p2)
ps.add(p2)
println(ps.size)
println(ps contains p2)
