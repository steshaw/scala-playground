//
// Another poor microbenchmark inspired by the Yammer/Scala debate.
//
// From http://www.infoq.com/news/2011/11/yammer-scala
//

object MapBench {
  val times = 100000
  def time(f: => Unit): Long = {
    val start = System.currentTimeMillis()
    f
    val end = System.currentTimeMillis()
    end - start
  }
  def scala() {
    val duration = time {
      val s = new collection.mutable.HashMap[Int, Int]();
      var i = 0
      while (i < times) {
        s.put(i, i)
        i = i + 1
      }
    }
    print(duration + " ")
  }
  def javaHashMap() {
    val duration = time {
      val m = new java.util.HashMap[Int, Int]
      var i = 0
      while (i < times) { 
        m.put(i, i)
        i = i + 1
      }
    }
    print(duration + " ")
  }
  def javaConcurrentHashMap() {
    val duration = time {
      val c = new java.util.concurrent.ConcurrentHashMap[Int, Int]
      var i = 0
      while (i < times) { 
        c.put(i, i)
        i = i + 1
      }
    }
    print(duration + " ")
  }
  def main(args: Array[String]) {
    print("scala.collection.mutable.HashMap: ")
    (1 to 10) foreach { i => scala() }
    println

    print("java.util.HashMap: ")
    (1 to 10) foreach { i => javaHashMap() }
    println

    print("java.util.concurrent.ConcurrentHashMap: ")
    (1 to 10) foreach { i => javaConcurrentHashMap() }
    println
  }
}
