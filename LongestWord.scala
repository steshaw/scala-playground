object LongestWord extends App {
  def longestWord(words: Array[String]) = {
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

  var longest = longestWord("The quick brown fox".split(" "))
  println(longest)

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
  val duration = timeIt(1000) {
    longest = longestWord(words)
  }
  println("took " + duration + "ms")
  println(longest + " length is " + longest._1.length)
}
