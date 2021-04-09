object Gundam1 {

  def face(a: Any): Unit = {}
  def start: Unit = {}
  def stop: Unit = {}
  class Boom extends Exception
  def triple_backflip = throw new Boom

  val north = "hi"
  val east = 5.6
  val south = new Exception("south")
  val west = 1

  def go(): Unit = {
    face(north)
    face(west)
    face(south)
    face(east)
    start
    stop
    triple_backflip
    face(-35)
  }

  def main(args: Array[String]): Unit = {
    try {
      go()
    } catch {
      case e: Boom => println("boom!")
    }
  }
}
