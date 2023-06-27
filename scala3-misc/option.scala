enum Option[+T]:
  case Some(x: T)
  case None

val n42 = Option.Some(42)
val none = Option.None

@main def main =
  {
    val x: Option[Int] = Option.Some(42)
    println(x)
  }
  {
    val x: Option[Int] = Option.None
    println(x)
  }
