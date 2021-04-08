/*

  [1, 2, 3]

  f (f (f init 3) 2) 1

 */
object FoldRightBug {

  def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B) = {
    import scala.collection.mutable.ArrayStack
    val s = new ArrayStack[A]
    fa.foreach(a => s += a)
    var r = z // z is forced, oops!
    while (!s.isEmpty) {
      // force and copy the value of r to ensure correctness
      val w = r
      r = f(s.pop, w)
    }
    r
  }

  def hi(a: => Int) {
    var unused = a
  }

  def main(args: Array[String]) {
    try {
      hi(sys.error("zap!"))
      println("avoided zap!")
    } catch {
      case e => println("oops: " + e)
    }

    try {
      println("init = bottom")
      val a = foldRight[Int, Int]((1 to 10).toList, sys.error("hi"))((a, b) => a)
      println(a)
    } catch {
      case e => println("oops: " + e)
    }

    try {
      println("fold -> summing")
      val a = foldRight[Int, Int => Int]((1 to 10).toList, identity)((a, b) => c => b.apply(a + c))
      println(a.apply(0))
    } catch {
      case e => println("oops: " + e)
                e.printStackTrace()
    }

    try {
      println("fold -> stringify summing")
      val a = foldRight[Int, String => String]((1 to 10).toList, identity)((a, b) => c => b.apply(a + c))
      println(a.apply("<<"))
    } catch {
      case e => println("oops: " + e)
                e.printStackTrace()
    }

    try {
      println("fold -> summing 2")
      val a = foldRight[Int, Int => Int]((1 to 10).toList, identity)((a, b) => b.compose(a + _))
      println(a.apply(0))
    } catch {
      case e => println("oops: " + e)
                e.printStackTrace()
    }

    try {
      println("fold -> stringify summing 2")
      val a = foldRight[Int, String => String]((1 to 10).toList, identity)((a, b) => b.compose(a + _))
      println(a.apply("<<"))
    } catch {
      case e => println("oops: " + e)
                e.printStackTrace()
    }

    try {
      println("fold -> consing")
      val a = foldRight[Int, List[Int]]((1 to 10).toList, List())(_ :: _)
      println(a)
    } catch {
      case e => println("oops: " + e)
    }

    try {
      println("fold -> function consing")
      val a = foldRight[Int, List[Int] => List[Int]]((1 to 3).toList, identity)((a, b) => b.compose(a :: _))
      println(a.apply(List()))
    } catch {
      case e => println("oops: " + e)
    }

    def go(n: Int) {
      println("n = " + n)
      val l1 = (1 to n).toList
      val a1 = l1.foldRight("")((a, b) => a.toString + b)
      val a2 = foldRight(l1, "")((a, b) => a.toString + b)
      println(a1 == a2)
    }
    for (i <- 1 to 20) {
      go(i * 1000)
    }
  }

}
