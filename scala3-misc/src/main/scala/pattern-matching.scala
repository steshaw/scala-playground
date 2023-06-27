def matchTest1(value: Any) = value match
  case str: String => println(s"Found a string: $str")
  case num: Int => println(s"Found an integer: $num")
  case other => println(s"Unknown value: $other")

// Pattern binding example (use of "@").
def matchTest2(value: Any) = value match
  case str: String => println(s"Found a string: $str")
  case num @ 42 => println(s"Found the number 42: $num")
  case other => println(s"Unknown value $other")

// Match types.
def matchTest3(value: Any): Unit = value match
  case list: List[t] => println(s"Found a list: $list")
  case set: Set[t] => println(s"Found a set: $set")
  case other => println("Unknown value: $other")

def processValue(value: Any): Unit = value match
  case str: String => println(s"Found a string: $str")
  case num: Int => println(s"Found an integer: $num")
  case _ => println("Unknown value")

def given1 =
  given Conversion[Double, Int] = _.toInt
  val doubleValue: Double = 3.14
  processValue(doubleValue) // Output: Found an integer: 3

def given2 =
  given Conversion[Double, Int] with {
    def apply(value: Double): Int = value.toInt
  }
  val doubleValue: Double = 3.14
  processValue(doubleValue) // Output: Found an integer: 3

def given3 =
  import scala.language.implicitConversions
  given Conversion[Double, Int] with {
    def apply(value: Double): Int = value.toInt
  }
  val doubleValue: Double = 3.14
  val intValue: Int = doubleValue // Does the implicit conversion.
  processValue(intValue) // Output: Found an integer: 3

object PatternMatching:
  def main(args: Array[String]) =
    val list = List(3, 42, "Hello world!", 'x', Nil)
    println("matchTest1")
    list.foreach(matchTest1)
    println("matchTest2")
    list.foreach(matchTest2)
    println("matchTest3")
    list.foreach(matchTest3)

    println("given1"); given1
    println("given2"); given2
    println("given3"); given3

