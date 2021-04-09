class Boom(s: String) extends Exception(s)

sealed abstract class Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

object Direction {
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

  def try_it(f: => Unit): Unit = {
    try {
      f
    } catch {
      case e @ (_: Boom | _: MatchError) => println(e)
    }
  }

  def doCmd(cmd: Command): Unit = {
    cmd match {
      case Face(_) => ()
      case Start => ()
      case Stop => ()
      case Chain(_, _) => ()
    }
  }

  implicit class Compose(cmd1: Command) {
    def ~>(cmd2: Command): Command =
      Chain(cmd1, cmd2)
  }

  val start = Start
  val stop = Stop
  def face(dir: Direction) = Face(dir)

  val north = North
  val east = East
  val south = South
  val west = West

  val startStop = start ~> stop

  def move(d: Direction) =
    face(d) ~> start ~> stop

  val cmds1 =
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

  val cmds2 = move(east) ~> move(west)

  val illegal1 = start ~> start
  val illegal2 = stop ~> stop
  val illegal3 = start ~> face(north)

  def go(): Unit = {
    doCmd(Face(North))
    doCmd(Face(West))
    doCmd(Face(South))
    doCmd(Face(East))
    doCmd(Start)
    doCmd(Stop)
  }

  def main(args: Array[String]): Unit = {
    println(Direction.label(West))
    // cmds1 and cmds2 seems equivalent but do so via
    // different nesting of Chains.
    assert(cmds1 != cmds2)
    println(cmds1)
    println(cmds2)
    go()
  }
}
