class Boom(s: String) extends Exception(s)

trait Direction

object Direction extends Enumeration {
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction
  def label(d: Direction) =
    d match {
      case North => "north"
      case East => "east"
      case South => "south"
      //case West => "west"
    }
}

object Gundam {
  import Direction._

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

  val north = North
  val east = East
  val south = South
  val west = West

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
    try_it(label(West))
    go()
  }
}
