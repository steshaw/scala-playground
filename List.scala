
trait List[+A] {
  def ::[B >: A](b: B): List[B] = Cons(b, this)
}
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def length[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case Cons(head, tail) => 1 + length(tail)
    }
}
