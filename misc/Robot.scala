//
// Adapted from http://lizdouglass.wordpress.com/2009/12/08/passing-by-name-in-scala/
//

class Robot(var xPosition: Int, var yPosition: Int, var direction: Direction) {
  def turnRight() {
    direction = direction.rightDirection
  }

  override def toString(): String = "%d %d %s".format(xPosition, yPosition, direction.abbreviation)
}

sealed abstract class Direction(val abbreviation: Char, val leftDirection: Direction, val rightDirection: Direction)
case object North extends Direction('N', West, East)
case object West extends Direction('W', South, North)
case object East extends Direction('E', North, South)
case object South extends Direction('S', East, West)

object RobotDemo extends App {
  for (d <- List(North, West, East, South)) {
    printf("%s left=%s right=%s\n", d, d.leftDirection, d.rightDirection)
  }
}
//RobotDemo.main(Array())
