package steshaw

object Paging {
  def getPage[A](as: List[A], pageNumber: Int, pageSize: Int) = as.drop((pageNumber - 1) * pageSize).take(pageSize)

  def main(args: Array[String]) = {
    val l = (1 to 1000).toList
    val pageSize = 6
    for (page <- -10 to 200) println("" + page + " - " + getPage(l, page, pageSize).mkString("[", ", ", "]"))

    val ignoreAsTooSlow = false
    if (!ignoreAsTooSlow) {
      val big = (1 to 100000).toList
      (1 to 10) foreach { i =>
        System.gc()
        val start = System.currentTimeMillis
        for (page <- -10 to 20000) getPage(big, page, pageSize).length
        System.err.println("took " + (System.currentTimeMillis - start) + "ms")
      }
    }
  }
}
