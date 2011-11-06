//
// Excercises from http://rafs-blog.blogspot.com/2008/01/scala-exercises.html
//

//
// map in terms of foldRight
//
def map[A, B](f: A => B)(xs: List[A]) = xs.foldRight(Nil: List[B])((n, acc) => f(n) :: acc)

{
  val r = map(((_:Int)+1))(List(1,2,3))
  println(r)
}

{
  val r = try {
    map(((_:Int)+1))((1 to 4500).toList).toString
  } catch {
    case e: StackOverflowError => "blew the stack!"
  }
  println(r)
}

//
// flatMap in terms of foldRight
//
def flatMap[A, B](f: A => List[B])(xs: List[A]) = xs.foldRight(Nil: List[B])((n, acc) => f(n) ::: acc)

{
  def repeat3(n: Int) = List.fill(3)(n)

  {
    val r = flatMap(repeat3)(List(1,2,3))
    println(r)
  }
  {
    val r = try {
      flatMap(repeat3)((1 to 4500).toList).toString
    } catch {
      case _: StackOverflowError => "blew the stack!"
    }

    println(r)
  }

  def benchmark(name: String, times: Int, a: => Unit) {
    import scala.compat.Platform
    try {
      printf("benchmark '%s':", name)
      for (i <- 1 to times) {
        Platform.collectGarbage
        val start = Platform.currentTime
        a
        val duration = Platform.currentTime - start
        Platform.collectGarbage
        printf(" %dms", duration)
      }
      println()
    } catch {
      case t: Throwable => printf("\n!!! benchmark '%s' failed with exception %s\n", name, t)
    }
  }

  // Compare our flatMap with built-in flatMap.
  def repeatMuch(n: Int) = List.fill(10000)(n)
  val bigList = (1 to 500).toList
  benchmark("our flatMap", 10, flatMap(repeatMuch)(bigList))
  benchmark("built-in flatMap", 10, bigList.flatMap(repeatMuch))
}

//
// concat in terms of foldRight
//
def concat[A](xs: List[A], ys: List[A]): List[A] = xs.foldRight(ys)((n, acc) => n :: acc)

{
  val r = concat(List(1,2,3), List(4,5,6))
  println(r)
}
{
  val r = try {
    concat((1 to 50000).toList, List(4,5,6)).toString
  } catch {
    case e: StackOverflowError => "blew the stack!"
  }
  println(r)
}

//
// concat using recursion
//
def concatr[A](xs: List[A], ys: List[A]): List[A] = xs match {
  case Nil => ys
  case x :: xs => x :: concatr(xs, ys)
}

{
  val r = concatr(List(1,2,3), List(4,5,6))
  println(r)
}
{
  val r = try {
    concatr((1 to 7000).toList, List(4,5,6)).toString
  } catch {
    case e: StackOverflowError => "blew the stack!"
  }
  println(r)
}

//
// maxOf - find max Int in list - generalised to T viewable as an Ordered[T].
//
def max[T <% Ordered[T]](a: T, b: T) = if (a > b) a else b
def maxOf[T <% Ordered[T]](xs: List[T]) = xs.reduceLeft((acc, n) => max(n, acc))

{
  val r = maxOf(List(6,3,1,9,8))
  println(r)
}
{
  val r = try {
    maxOf(scala.util.Random.shuffle((1 to 7000).toList)).toString
  } catch {
    case e: StackOverflowError => "blew the stack!"
  }
  println(r)
}
