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

@main def main =
  val list = List(3, 42, "Hello world!", 'x', Nil)
  println("matchTest1")
  list.foreach(matchTest1)
  println("matchTest2")
  list.foreach(matchTest2)
  println("matchTest3")
  list.foreach(matchTest3)

