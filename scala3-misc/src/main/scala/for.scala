object For:
  def main(args: Array[String]) =
    val xs = List(1,2,3)
    val ys = List('a', 'b', 'c')

    for
      x <- xs
      y <- ys
    do println((x, y))

    val r = for
      x <- xs
      y <- ys
    yield (x, y)

    println(r)
