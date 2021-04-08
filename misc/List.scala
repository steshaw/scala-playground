//
// An excercise to build a functional/immutable list.
//
// Note: The excercise was to observe the type signatures (not to be concerned with performance).
//
// Trying an invariant list.
//

package steshaw.collection

sealed abstract class List[A] {
  //def ::[B >: A](b: B): List[B] = Cons(b, this)
  def ::(a: A): List[A] = Cons(a, this)

  def length: Int = this match {
    case Nil() => 0
    case Cons(head, tail) => 1 + tail.length
  }

  def append(bs: List[A]): List[A] = this match {
    case Nil() => bs
    case Cons(a, as) => Cons(a, as.append(bs))
  }

  def flatMap[B](f: A => List[B]): List[B] = this match {
    case Nil() => Nil()
    case Cons(a, as) => f(a).append(as.flatten(f))
  }

  def flatten[B](implicit f: A => List[B]): List[B] = flatMap(f)
}
case class Cons[A](head: A, tail: List[A]) extends List[A]
case class Nil[A]() extends List[A]

object List {
  def ::[A](a: A, as: List[A]): List[A] = ::(a, as)

  def apply[T](ts: T*): List[T] = (ts :\ (Nil(): List[T]))((head, tail) => Cons(head, tail))

  def flatten[A](xss: List[List[A]]): List[A] = xss match {
    case Nil() => Nil()
    case Cons(xs, xss) => append(xs, (flatten(xss)))
  }

  def length[A](xs: List[A]): Int = xs match {
    case Nil() => 0
    case Cons(head, tail) => 1 + length(tail)
  }

  def append[A](as: List[A], bs: List[A]): List[A] = as match {
    case Nil() => bs
    case Cons(a, as) => Cons(a, as.append(bs))
  }
}

object Demo {
  val as = List(1,2,3)
  val bs = List(4,5)
  val cs = List(6)
  val ds = Nil[Int]()
  val xss = List(as, ds, bs, ds, cs, ds)
  val flattened1 = List.flatten(xss)
  val flattened2 = xss.flatten
}
