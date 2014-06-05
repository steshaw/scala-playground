
object ex2 {

  def main(args: Array[String]) {
    case class Building(start: Int, end: Int, height: Int) {
      def width = end - start
    }

    def lowerThan(height: Int) = (b: Building) => b.height < height
    val lowerThanSix = lowerThan(6)
    def widerThan(width: Int) = (b: Building) => b.width > width

    val bs: List[Building] = List(Building(0, 3, 1), Building(0, 4, 1))

    {
      def and(p1: Building => Boolean, p2: Building => Boolean) = (b: Building) => p1(b) && p2(b)

      Console.println("lowerThanSix = " + bs.filter(lowerThanSix))

      var r = bs.filter(and(lowerThanSix, widerThan(3)))
      Console.println("r = " + r)
    }

/*

 FIX: Broken due to something about needing to import structual typing?

    {
      implicit val foo = (p1: Building => Boolean) => {
        def and(p2: Building => Boolean) = (b: Building) => p1(b) && p2(b)
      }
      import language.implicitConversions
      import language.reflectiveCalls
      import language.dynamics
      val r2 = bs.filter(lowerThanSix and widerThan(3))
      Console.println("r2 = " + r2)
    }
*/

    {
      case class Fred(p1: Building => Boolean) {
        def and(p2: Building => Boolean) = (b: Building) => p1(b) && p2(b)
      }

      implicit val bar = (p1: Building => Boolean) => Fred(p1)

      val r3 = bs.filter(lowerThanSix and widerThan(3))
      Console.println("r3 = " + r3)
    }
  }

}
