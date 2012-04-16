import scala.util.continuations._

def doSomething1 = reset {
  println("Ready?")
  val result = 1 + gotcha * 3
  println(result)
}

def gotcha = shift {
  k: (Int => Unit) => println(99); "Gotcha!"
}

val s = doSomething1
println("result was " + s)
