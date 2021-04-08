//
// Inspired by http://vimeo.com/20290504
//
object FunctionalProgramming {

  def computation1() = (1 to 1000000)
    .map(_ * 2)
    .filter(_ > 100000)
    .reduceLeft(_ + _)

  def computation2() = (1 to 1000000)
    .view
    .map(_ * 2)
    .filter(_ > 100000)
    .reduceLeft(_ + _)

  def timeStamp = System.nanoTime

  def timeOnce[A](f: => A) = {
    val start = timeStamp
    val result = f
    val duration = timeStamp - start
    println("Computation time: " + (duration / 1e6) + "ms")
    result
  }

  implicit def implicitTimes(n: Int) = new {
    def times(f: => Unit) = (1 to n).foreach { ignore =>
      f
    }
  }

  def main(args: Array[String]) {
    List(
      ("computation1", () => computation1),
      ("computation2", () => computation2)
      ) foreach { case (name, c) =>
        println(name); 10.times {timeOnce(c())}
      }
  }
}
