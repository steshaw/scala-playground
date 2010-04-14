package steshaw

/**
 * Now using while loop in code that gets timed for closer comparison with Java version.
 * HAH! In fact using the while loop now seems slower than using a for comprehension! Yah Scala 2.8 Beta1!
 * Well, at least it's so hard to measure the "slowness" of the for comprehension that it's comparable to a while loop.
 */
object Paging3 {
  def getPage[A](as: List[A], pageNumber: Int, pageSize: Int) = as.view.drop((pageNumber - 1) * pageSize).take(pageSize)

  def main(args: Array[String]) = {
    val l = (1 to 1000).toList
    val pageSize = 6
    for (page <- -10 to 200) println("" + page + " - " + getPage(l, page, pageSize).mkString("[", ", ", "]"))

    val big = (1 to 100000).toList
    (1 to 10) foreach { i =>
      System.gc()
      val start = System.currentTimeMillis
      var page = -10
      while (page <= 20000) {
        getPage(big, page, pageSize).length
        page += 1
      }
      System.err.println("took " + (System.currentTimeMillis - start) + "ms")
    }
  }
}
