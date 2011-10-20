import scala.collection.mutable

def countWords(text: String) = {
  val counts = mutable.Map[String, Int]()
  for (word <- text.split("[ ,!.]+")) {
    val lowerWord = word.toLowerCase
    val oldCount = counts.getOrElse(lowerWord, 0)
    counts(lowerWord) = oldCount + 1
  }
  counts
}

val text = "See Spot run. Run, Spot. Run!"

countWords(text) foreach println
