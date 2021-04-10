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

case class Command(
  order: String,
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
      case Command(name, optDir) =>
        name match {
          case "face" => optDir match {
            case Some(dir) => ()
            case None => throw new Boom("no direction!")
          }
          case "start" => ()
          case "stop" => ()
        }
    }
  }

  val start = Command("start", Some(North))
  val stop = Command("stop", Some(North))
  val face_north = Command("face", Some(North))
  val face_east = Command("face", Some(East))
  val face_south = Command("face", Some(South))
  val face_west = Command("face", Some(West))
  val triple_backflip1 =
    Command("triple backflip!", Some(South))
  val triple_backflip2 =
    Command("triple backflip!", None)
  val garbage = Command("garbage", None)

  def go(): Unit = {
    do_cmd(face_north)
    do_cmd(face_west)
    do_cmd(face_south)
    do_cmd(face_east)
    do_cmd(start)
    do_cmd(stop)
    try_it(do_cmd(triple_backflip1))
    try_it(do_cmd(triple_backflip2))
    try_it(do_cmd(garbage))
  }

  def main(args: Array[String]): Unit = {
    println(label(West))
    go()
  }
}
