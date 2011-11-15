object BubbleSort extends App {

  def bubbleSort(a: Array[Int]) {
    var swapped: Boolean = false
    do {
      swapped = false
      for (i <- 0 until a.length - 1) {
        if (a(i) > a(i+1)) {
          // Swap them.
          val temp = a(i)
          a(i) = a(i+1)
          a(i+1) = temp
          swapped = true
        }
      }
    } while (swapped);
  }

  var a = (1 to 10).reverse.toArray
  println(a.toSeq)
  bubbleSort(a)
  println(a.toSeq)

  val rand = new scala.util.Random()
  val ra = Array.fill(10)(rand.nextInt(10) + 1)
  println(ra.toSeq)
  bubbleSort(ra)
  println(ra.toSeq)
}
