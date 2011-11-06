//
// Excercises from http://rafs-blog.blogspot.com/2008/01/scala-exercises.html
//

// map in terms of foldRight
def map[A, B](f: A => B)(xs: List[A]) = xs.foldRight(Nil : List[B])((n, acc) => f(n) :: acc)

{
  val r = map(((_:Int)+1))(List(1,2,3))
  println(r)
}

{
  val r = try {
    map(((_:Int)+1))((1 to 5000).toList).toString
  } catch {
    case e: StackOverflowError => "blew the stack!"
  }
  println(r)
}

// TODO concat in terms of foldRight

// TODO concat using recursion

// TODO find max Int in list
