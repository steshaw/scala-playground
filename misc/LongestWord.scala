object LongestWord extends App {
  def longestWord1(words: Array[String]) = {
    var word = words(0)
    var idx = 0
    for (i <- 1 until words.length) {
      if (words(i).length > word.length) {
        word = words(i)
        idx = i
      }
    }
    (word, idx)
  }

  def longestWord2(words: Array[String]) = {
    def wordIndexOrdering = new Ordering[(String, Int)] {
      def compare(a: (String, Int), b: (String, Int)) = a._1.length compare b._1.length
    }
    words.zipWithIndex.max(wordIndexOrdering)
  }

  def longestWord3(words: Array[String]) =
    words.zipWithIndex.reduceLeft((acc, item) => if (acc._1.length < item._1.length) item else acc)

  val longest1 = longestWord1("The quick brown fox".split(" "))
  println(longest1)
  val longest2 = longestWord2("The quick brown fox".split(" "))
  println(longest2)

  def time = scala.compat.Platform.currentTime

  def timeIt(numTimes: Long)(action: => Unit) = {
    var start = time
    var i = 0
    while (i < numTimes) {
      action
      i += 1
    }
    time - start
  }

  val words = io.Source.fromFile("/usr/share/dict/words").getLines.toArray

  def timeLongestWord(longestWord: Array[String] => (String, Int)) {
    (1 to 3) foreach { i =>
      var longest: (String, Int) = ("", 0)
      val duration = timeIt(10) {
        longest = longestWord(words)
      }
      println("took " + duration + "ms")
      println(longest + " length is " + longest._1.length)
    }
  }

  println("longestWord1")
  timeLongestWord(longestWord1)
  println

  println("longestWord2")
  timeLongestWord(longestWord2)
  println

  println("longestWord3")
  timeLongestWord(longestWord3)
}
