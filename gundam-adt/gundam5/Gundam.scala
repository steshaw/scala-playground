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
case class Chain(cmd1: Command, cmd2: Command) extends Command

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
      case Face(_) => ()
      case Start => ()
      case Stop => ()
      case Chain(_, _) => ()
    }
  }

  val instructions =
    Chain(
      Chain(
        Chain(
          Face(East),
          Chain(
            Start,
            Stop
          )
        ),
        Face(West)
      ),
      Chain(
        Start,
        Stop
      )
    )

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
