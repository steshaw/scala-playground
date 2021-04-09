class Boom(s: String) extends Exception(s)

sealed abstract class Direction

object Direction {
  case object North extends Direction
  case object East extends Direction
  case object South extends Direction
  case object West extends Direction
  def label(d: Direction) =
    d match {
      case North => "north"
      case East => "east"
      case South => "south"
      case West => "west"
    }
}

sealed trait Order
case object Face extends Order
case object Start extends Order
case object Stop extends Order

case class Command(
  order: Order,
  dir: Option[Direction]
)

object Gundam {
  import Direction._

  def try_it(f: => Unit): Unit = {
    try {
      f
    } catch {
      case e @ (_: Boom | _: MatchError) => println(e)
    }
  }

  def do_cmd(cmd: Command): Unit = {
    cmd match {
      case Command(Face, Some(dir)) => ()
      case Command(Start, None) => ()
      case Command(Stop, None) => ()
      case _ => throw new Boom("not ruled out!")
    }
  }

  val start = Command(Start, None)
  val stop = Command(Stop, None)
  val face_north = Command(Face, Some(North))
  val face_east = Command(Face, Some(East))
  val face_south = Command(Face, Some(South))
  val face_west = Command(Face, Some(West))
  val garbage1 = Command(Face, None)
  val garbage2 = Command(Start, Some(North))

  def go(): Unit = {
    do_cmd(face_north)
    do_cmd(face_west)
    do_cmd(face_south)
    do_cmd(face_east)
    do_cmd(start)
    do_cmd(stop)
    try_it(do_cmd(garbage1))
    try_it(do_cmd(garbage2))
  }

  def main(args: Array[String]): Unit = {
    println(label(West))
    go()
  }
}
