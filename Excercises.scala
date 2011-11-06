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
// TODO flatMap in terms of foldRight
//

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
    concat((1 to 4500).toList, List(4,5,6)).toString
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
// find max Int in list - generalised to T viewable as an Ordered[T].
//
def max[T <% Ordered[T]](xs: List[T]) = xs match {
  case Nil => sys.error("cannot take the max of an empty list")
  case x :: xs => xs.foldLeft(x)((acc, n) => if (n > acc) n else acc)
}
{
  val r = max(List(6,3,1,9,8))
  println(r)
}
{
  val r = try {
    max(scala.util.Random.shuffle((1 to 7000).toList)).toString
  } catch {
    case e: StackOverflowError => "blew the stack!"
  }
  println(r)
}
