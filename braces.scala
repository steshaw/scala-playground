def times(n: Int)(block: => Unit) =
  for (i <- 1 to n) {
    block
  }

@main def main =
  times(3):
    println("ah")
    println("ha")

  val r = List(1,2,3).map:
    n => n + 10
  println(r)
