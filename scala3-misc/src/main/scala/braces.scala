def times(n: Int)(block: => Unit) =
  (1 to n).foreach(_ => block)

object Braces:
  def main(args: Array[String]) =
    times(3):
      println("ah")
      println("ha")

    val r = List(1,2,3).map:
      n => n + 10
    println(r)
