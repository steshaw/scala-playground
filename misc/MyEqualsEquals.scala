object MyEqualsEquals extends App {

  //
  // Implements '==' in Scala rather than pseudo Scala as does
  // https://lampsvn.epfl.ch/trac/scala/browser/scala/tags/R_2_9_1_final/src/library-aux/scala/AnyRef.scala#L76
  //
  class MyEqualsEquals(o: Any) {
    def ===(that: Any): Boolean = {
      val this1 = o.asInstanceOf[AnyRef]
      val that2 = that.asInstanceOf[AnyRef]
      if (this1 eq null) that2 eq null
      else o equals that
    }
  }
  implicit def any2MyEqualsEquals(o: Any) = new MyEqualsEquals(o)

  case class Person(val name: String, val age: Int)

  val s = "Fred"
  val n: String = null

  def go[T](block: => T): Unit = {
    import scala.util.control.Exception.catching
    println(catching(classOf[NullPointerException]) opt block)
  }

  go (s equals n)
  go (n equals s)

  def both(a: Any, b: Any): Unit = {
    println("-" * 10)
    println(a + " == " + b)
    println("orig:" + (a == b))
    println(" new:" + (a === b))
  }

  both(s, n)
  both(n, s)
  both("a", "a")
  both(1, 2)
  both(1, 1)

  both(Person("Fred", 40), Person("Barney", 50))
  both(Person("Fred", 40), Person("Fred", 50))
  both(Person("Fred", 40), Person("Fred", 40))
}
