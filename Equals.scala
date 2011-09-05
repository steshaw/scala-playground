import scala.util.control.Exception.catching

object Equals extends App {
  val s: String = null
  println(catching(classOf[NullPointerException]) opt {s equals "Fred"}) // Would boom! but catching returns None.
  println(catching(classOf[NullPointerException]) opt {"Fred" equals s})
  println(catching(classOf[NullPointerException]) opt {s == "Fred"})
  println(catching(classOf[NullPointerException]) opt {"Fred" == s})
}
