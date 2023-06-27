enum Option[+T]:
  case Some(x: T) extends Option[T]
  case None       extends Option[Nothing]

@main def main =
  {
    val x: Option[Int] = Option.Some(42)
    println(x)
  }
  {
    val x: Option[Int] = Option.None
    println(x)
  }
