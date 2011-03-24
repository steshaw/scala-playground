//
// Inspired by http://vimeo.com/20290504
//
object FunctionalProgramming {

  def thing = (1 to 1000000)
    .map(_ * 2)
    .filter(_ > 100000)
    .reduceLeft(_ + _)

  def timeStamp = System.currentTimeMillis

  def timeOnce(f: => Unit) = {
    val start = timeStamp
    f
    timeStamp - start
  }

  implicit def implicitTimes(n: Int) = new {
    def times(f: => Unit) = (1 to n).foreach { ignore =>
      f
    }
  }

  def main(args: Array[String]) {
    if (true) {
      println(timeOnce(thing))
    } else {
      10.times {
        println(timeOnce(thing).toFloat / 1000)
      }
    }
  }
}
