def matchTest(value: Any) =
  print("1st: ")
  value match
    case str: String => println(s"Found a string: $str")
    case num: Int => println(s"Found an integer: $num")
    case other => println(s"Unknown value: $other")

  print("2nd: ")
  value match
    case str: String => println(s"Found a string: $str")
    case num @ 42 => println(s"Found the number 42: $num")
    case other => println(s"Unknown value $other")

@main def main =
  matchTest(3)
  matchTest(42)
  matchTest("Hello World")
  matchTest('x')
  matchTest(Nil)
