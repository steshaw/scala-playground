//
// Another terrible microbenchmark from the Yammer/Scala debate.
//
// http://www.infoq.com/news/2011/11/yammer-scala
//
// Note: It's to be expect that for comprehensions are slower than while loops in Scala. 
//       For comprehensions over ranges are not optimised into loops.  This used to be well known.
//

object ForWhileBench extends App {
  val n = 1000000000

  def timed[T](name: String)(f: => T) = {
    printf("%s :\n", name)
    val t = System.nanoTime
    val r = f
    val d = System.nanoTime - t
    printf("%,d ns\n", d)
    printf("%,d ops/s\n", (n * 1000000000L) / d)
    printf("%,f ns/op\n", d.toFloat / n)
  }

  timed("while") {
    var sum = 0L; 
    var i = 1; 
    while (i <= n) { 
      sum += i
      i += 1
    }  
    sum
  }

  timed("for") {
    var sum = 0L; 
    for (i <- 1 to n) { 
      sum += i 
    }
    sum
  }
}
