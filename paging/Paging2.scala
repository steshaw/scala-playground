package steshaw

/**
 * getPage now using ".view" to get lazy behaviour and performance boost
 */
object Paging2 {
  def getPage[A](as: List[A], pageNumber: Int, pageSize: Int) = as.view.drop((pageNumber - 1) * pageSize).take(pageSize)

  def main(args: Array[String]) = {
    val l = (1 to 1000).toList
    val pageSize = 6
    for (page <- -10 to 200) println("" + page + " - " + getPage(l, page, pageSize).mkString("[", ", ", "]"))

    val big = (1 to 100000).toList
    (1 to 10) foreach { i =>
      System.gc()
      val start = System.currentTimeMillis
      for (page <- -10 to 20000) {
        getPage(big, page, pageSize).length // Calling length to force the computation.
      }
      System.err.println("took " + (System.currentTimeMillis - start) + "ms")
    }
  }
}
