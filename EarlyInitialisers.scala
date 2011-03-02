//
// See http://stackoverflow.com/questions/4712468/in-scala-what-is-an-early-initializer
//

abstract class X {
  val name: String
  val size = name.size
}

class Y extends {
  val name = "class Y"
}

class Z extends X {
  val name = "class Z"
}

println(new Y())
println(new Z()) // explodes!
