//
// Paul Phillips posted the following code to scala-lang mailing list 24-Feb-2011.
//
// See https://groups.google.com/d/msg/scala-language/HynWDcGkVH8/PFGcom94SzkJ
//
// Replaced "sys.error" with just "error" (which is defined in Predef).
//

object Stm {
  def cons[A](_hd: => A, _tl: => Stm[A]): Stm[A] = new Stm[A] {
    lazy val hd = _hd
    lazy val tl = _tl
  }
  def empty[A] : Stm[A] = new Stm[A] {
    override def isEmpty = true
    def hd = error("head of empty Stm.")
    def tl = error("tail of empty Stm.")
  }
}

abstract class Stm[+A] {
  outer =>

  def hd: A
  def tl: Stm[A]

  def **[A1 >: A](elem: => A1): Stm[A1] = new Stm[A1] {
    lazy val hd = elem
    lazy val tl = outer
  }

  def ::[A1 >: A](elem: => A1): Stm[A1] = new Stm[A1] {
    lazy val hd = elem
    lazy val tl = outer
  }
  def drop(num: Int): Stm[A] = if (num == 0) this else tl.drop(num - 1)
  def take(num: Int): Stm[A] = if (num == 0) Stm.empty[A] else Stm.cons(hd, take(num - 1))
  def isEmpty = false
  def toList: List[A] = if (isEmpty) Nil else hd :: tl.toList
  def size: Int = if (isEmpty) 0 else 1 + tl.size
  def apply(idx: Int): A = drop(idx).hd
}

object Test {
  def f(num: Int) = (num to 1 by -1).foldLeft(Stm.empty[Int])((xs, x) =>
    if (x % 10 == 0) xs ** { println("x = " + x) ; error("divisible by 10") }
    else xs ** x
  )

  def main(args: Array[String]): Unit = {
    val xs = f(99)
    println(xs.size)
    println((xs take 9 toList).sum + (xs drop 10 take 9 toList).sum)
    // boom
    println(xs(9))
  }
}
