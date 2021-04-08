
trait D // Oddly, D can be defined as a class but then C can never be instantiated.

class C { self: D =>
}

object SelfType extends App {
  val c = new C with D
}
