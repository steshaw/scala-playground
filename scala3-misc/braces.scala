def times(n: Int)(block: => Unit) =
  (1 to n).foreach(_ => block)

@main def main =
  times(3):
    println("ah")
    println("ha")

  val r = List(1,2,3).map:
    n => n + 10
  println(r)
