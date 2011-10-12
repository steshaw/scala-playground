//
// From "Changes in Version 2.1.8 (23-Aug-2006)"
// See http://www.scala-lang.org/node/43
//

val p = List(1, 2, 3)
val q = List(1, 2)
val r = q
r match {
  case _: p.type => println("p")
  case _: q.type => println("q")
}
