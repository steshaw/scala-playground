object Chapter3 extends App {

  def randomArray(n: Int) = Array.fill(n)(util.Random.nextInt(n))

  implicit def array2myarray[T](a: Array[T]) = new Object {
    override def toString = a.mkString("[", ", ", "]")
    def show = toString
  }

  println(randomArray(10).show)
  println(randomArray(10).show)
  println(randomArray(10).show)
  println(randomArray(3).show)
  println(randomArray(3).show)
  println(randomArray(3).show)

  def swapAdjacent(a: Array[Int]) = {
    for (i <- 1 until (a.size, 2)) {
      val tmp = a(i - 1)
      a(i - 1) = a(i)
      a(i) = tmp
    }
    a
  }

  def br() = println("-" * 10)

  println(swapAdjacent(Array()).show)
  br()
  println(swapAdjacent(Array(1)).show)
  br()
  println(swapAdjacent(Array(1, 2)).show)
  br()
  println(swapAdjacent(Array(1, 2, 3)).show)
  br()
  println(swapAdjacent(Array(1, 2, 3, 4)).show)
  br()
  println(swapAdjacent(Array(1, 2, 3, 4, 5)).show)
  br()

  def swapAdjacent_(a: Array[Int]) = {
    val result = a.clone
    val b = for (i <- 1 until (a.size, 2)) {
      val tmp = a(i - 1)
      result(i - 1) = a(i)
      result(i) = tmp
    }
    result
  }

  println(swapAdjacent_(Array()).show)
  br()
  println(swapAdjacent_(Array(1)).show)
  br()
  println(swapAdjacent_(Array(1, 2)).show)
  br()
  println(swapAdjacent_(Array(1, 2, 3)).show)
  br()
  println(swapAdjacent_(Array(1, 2, 3, 4)).show)
  br()
  println(swapAdjacent_(Array(1, 2, 3, 4, 5)).show)
  br()

  def posNegPartition(a: Array[Int]) = {
    val (a1, a2) = a.partition(_ > 0)
    a1 ++ a2
  }

  def posNegSort(a: Array[Int]) = {
    def negPosZero(n: Int) = {
      val result = n.signum
      if (result == 0) -1 else result
    }
    a.sortWith(negPosZero(_) > negPosZero(_))
  }

  def randomArray2(n: Int) = Array.fill(n)(util.Random.nextInt(10) - 5)

  def tryIt() {
    val a = randomArray2(7)
    val r1 = posNegPartition(a)
    val r2 = posNegSort(a)
    printf("%s => \n  %s\n  %s\n  %s\n", a.show, r1.show, r2.show, r1 sameElements r2)
  }

  (1 to 10).foreach { n =>
    tryIt()
  }

  def average(a: Array[Double]) = a.sum / a.length

  def showAverage(a: Array[Double]) = printf("%s.average => %s\n", a.show, average(a))

  showAverage(Array(1.0, 2.0))
  showAverage(Array(2.0, 3.0))
  showAverage(Array(2.0, 3.0, 1.0))
  br()


  import collection.SeqLike
  import collection.mutable.Buffer

  def reverseSort[Repr](a: SeqLike[Int, Repr]) = a.sortWith(_ > _)

  val a = Array(5,6,7, 1,2,3)
  val b = Buffer(5,6,7, 1,2,3)
  println(reverseSort(a).show)
  println(reverseSort(b))
  br()

  println(Array(1,1,2,3,1,2,3,1,2,3).distinct.show)
  br()

  (java.util.TimeZone.getAvailableIDs 
     filter (_.startsWith("America/")) map (_.replace("America/", "")) sorted) foreach println


  import java.awt.datatransfer._
  import collection.JavaConversions._

  val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
  val natives: Buffer[String] = flavors.getNativesForFlavor(DataFlavor.imageFlavor)
  println(natives)
}
