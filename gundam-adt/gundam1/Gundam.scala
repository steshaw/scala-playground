class Boom(s: String) extends Exception(s)

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Gundam {

  def face(a: Any): Unit = {
    a match {
      case a : Int => {
        if (a < 0)
          throw new Boom("negative!")
      }
      case _ => ()
    }
  }
  def start: Unit = {}
  def stop: Unit = {}

  def triple_backflip: Unit = throw new Boom("triple backflip")

  private val north = "hi"
  private val east = 5.6
  private val south = new Exception("south")
  private val west = 1

  def try_it(f: => Unit): Unit = {
    try {
      f
    } catch {
      case e: Boom => println(e)
    }
  }

  def go(): Unit = {
    face(north)
    face(west)
    face(south)
    face(east)
    start
    stop
    try_it(triple_backflip)
    try_it(face(-35))
  }

  def main(args: Array[String]): Unit = {
    go()
  }
}
