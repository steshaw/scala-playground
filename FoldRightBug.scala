object FoldRightBug {
  def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B) = {
    import scala.collection.mutable.ArrayStack
    val s = new ArrayStack[A]
    fa.foreach(a => s += a)
    var r = z
    while (!s.isEmpty) {r = f(s.pop, r)}
    r
  }

  def hi(a: =>Int) {
    lazy var unused = a
    println("hi")
  }

  def main(args: Array[String]) {
    hi(sys.error("zap!"))
    sys.exit(0);
    val a = foldRight[Int, Int]((1 to 10).toList, sys.error("hi"))((a, b) => a)
    println(a)
    println("hi")
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
