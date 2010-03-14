//
// Adapted from http://www.scala-lang.org/node/114
//

abstract trait SemiGroup[A] {
  def add(x: A, y: A): A
}
abstract trait Monoid[A] extends SemiGroup[A] {
  def unit: A
}
object Monoids {
  implicit object StringMonoid extends Monoid[String] {
    def add(x: String, y: String): String = x concat y
    def unit: String = ""
  }
  implicit object IntMonoid extends Monoid[Int] {
    def add(x: Int, y: Int): Int = x + y
    def unit: Int = 0
  }
}
object MonoidTest extends Application {
  def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
    if (xs.isEmpty) m.unit
    else m.add(xs.head, sum(xs.tail))

  import Monoids._
  println(sum(List(1, 2, 3)))
  println(sum(List("a", "b", "c")))
}
