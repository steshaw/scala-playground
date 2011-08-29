object Chapter4 extends App {

  import scala.collection.JavaConversions._

  val gizmos = Map(
    "iPad2" -> ("GBP", 399.00),
    "iPhone" -> ("GBP", 612.00)
  )

  val discounted =
    for ((name, (currency, price)) <- gizmos)
      yield (name -> (currency, price * 0.9))

  println(gizmos)
  println(discounted)

  import collection.mutable.HashMap

  def countWords() = {
    val words = HashMap[String, Int]()

    def process(word: String) {
      val count = words.getOrElse(word, 0)
      words(word) = count + 1
    }
    val in = new java.util.Scanner(new java.io.File("Chapter4.scala"))
    while (in.hasNext()) {
      process(in.next())
    }
    words
  }
  val words = countWords()
  words foreach { case (word, count) => printf("%5d %s\n", count, word) }
  println("=" * 30)

  def countWordsImmutable() = {
    var words = Map[String, Int]()

    def process(word: String) {
      val count = words.getOrElse(word, 0)
      words = words + (word -> (count + 1))
    }
    val in = new java.util.Scanner(new java.io.File("Chapter4.scala"))
    while (in.hasNext()) {
      process(in.next())
    }
    words
  }
  val wordsImmutable = countWordsImmutable()
  wordsImmutable foreach { case (word, count) => printf("%5d %s\n", count, word) }
  println("=" * 30)
  println(wordsImmutable == words)

  def countWordsSorted() = {
    var words = collection.immutable.SortedMap[String, Int]()

    def process(word: String) {
      val count = words.getOrElse(word, 0)
      words = words + (word -> (count + 1))
    }
    val in = new java.util.Scanner(new java.io.File("Chapter4.scala"))
    while (in.hasNext()) {
      process(in.next())
    }
    words
  }
  val wordsSorted = countWordsSorted()
  wordsSorted foreach { case (word, count) => printf("%5d %s\n", count, word) }
  println("=" * 30)
  println(wordsSorted == words)

  def countWordsTreeMap() = {
    val words: scala.collection.mutable.Map[String, Int] = 
      new java.util.TreeMap[String, Int]

    def process(word: String) {
      val count = words.getOrElse(word, 0)
      words(word) = count + 1
    }
    val in = new java.util.Scanner(new java.io.File("Chapter4.scala"))
    while (in.hasNext()) {
      process(in.next())
    }
    words
  }
  val wordsTreeMap = countWordsTreeMap()
  wordsTreeMap foreach { case (word, count) => printf("%5d %s\n", count, word) }
  println(wordsTreeMap == words)
  println("=" * 30)

  val days = collection.mutable.LinkedHashMap(
    "Sunday" -> java.util.Calendar.SUNDAY,
    "Monday" -> java.util.Calendar.MONDAY,
    "Tuesday" -> java.util.Calendar.TUESDAY,
    "Wednesday" -> java.util.Calendar.WEDNESDAY,
    "Thursday" -> java.util.Calendar.THURSDAY,
    "Friday" -> java.util.Calendar.FRIDAY,
    "Saturday" -> java.util.Calendar.SATURDAY
  )
  days foreach println

  val props: collection.mutable.Map[String, String] = System.getProperties()
  //props foreach { case (name, value) => printf("%-30s | %s\n", name, value) }

  val longestLength = props.keys.map(_.length).max
  println(longestLength)
  println(props.keys.filter(_.length == longestLength))

  props foreach { case (name, value) =>
    printf("%s | %s\n", name.padTo(longestLength, ' '), value)
  }

  def minmax(a: Array[Int]) = (a.min, a.max)

  println(minmax(Array(1,2,3,4,5,6)))

  def lteqgt(a: Array[Int], n: Int) = {
    (a.count(_ < n), a.count(_ == n), a.count(_ > n))
  }

  println(lteqgt(Array(1,2,3,4,5,6), 4))
}
