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

sealed trait Command
case class Face(dir: Direction) extends Command
case object Start extends Command
case object Stop extends Command

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
      case Face(dir) => ()
      case Start => ()
      case Stop => ()
      case _ => throw new Boom("not ruled out!")
    }
  }

  def go(): Unit = {
    do_cmd(Face(North))
    do_cmd(Face(West))
    do_cmd(Face(South))
    do_cmd(Face(East))
    do_cmd(Start)
    do_cmd(Stop)
  }

  def main(args: Array[String]): Unit = {
    println(label(West))
    go()
  }
}
