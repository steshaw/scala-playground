@main def main =
  val value: Any = 42

  value match {
    case str: String => println(s"Found a string: $str")
    case num: Int => println(s"Found an integer: $num")
    case _ => println("Unknown value")
  }
