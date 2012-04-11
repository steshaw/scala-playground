//
// See https://wiki.scala-lang.org/display/SW/Kleisli+Monad
//

val f: String => List[File]
val g: File => List[Trade]
// I now clearly have a function h:

val h: String => List[Trade]

// Which is "f flatMap g". Except of course that you cannot compose f and g in this point-free style in scala.

h = kleisli(f) >>= g
