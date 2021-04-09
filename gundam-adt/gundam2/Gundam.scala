class Boom(s: String) extends Exception(s)

object Direction extends Enumeration {
  val North, East, South, West = Value

  def label(d: Value) =
    d match {
      case North => "north"
      case East => "east"
      case South => "south"
      //case West => "west"
    }
}

object Gundam {

  def face(a: Any): Unit = {
    a match {
      case a : Int => {
        if (a < 0) throw new Boom("negative!")
      }
      case _ => ()
    }
  }
  def start: Unit = {}
  def stop: Unit = {}
  def triple_backflip = throw new Boom("triple backflip")

  val north = Direction.North
  val east = Direction.East
  val south = Direction.South
  val west = Direction.West

  def try_it(f: => Unit): Unit = {
    try {
      f
    } catch {
      case e @ (_: Boom | _: MatchError) => println(e)
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
    import Direction._
    try_it(label(West))
    go()
  }
}
