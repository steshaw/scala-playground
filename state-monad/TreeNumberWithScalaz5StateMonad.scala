package steshaw

object Scalaz5 extends States // Subset of Scalaz5

sealed trait State[S, +A] { // From Scalaz5
  def apply(s: S): (S, A)

  import Scalaz5._

  def map[B](f: A => B): State[S, B] = state(apply(_) match {
    case (s, a) => (s, f(a))
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = state(apply(_) match {
    case (s, a) => f(a)(s)
  })

  def !(s: S) = apply(s)._2

  def ~>(s: S) = apply(s)._1

  def withs(f: S => S): State[S, A] = state(f andThen (apply(_)))
}

trait States { // From Scalaz5
  def state[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    def apply(s: S) = f(s)
  }

  def init[S]: State[S, S] = state[S, S](s => (s, s))

  def modify[S](f: S => S) = init[S] flatMap (s => state(_ => (f(s), ())))
}

object TreeNumberWithScalaz5StateMonad {

  sealed abstract class Tree[+A]
  final case class Leaf[A](a: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  import Scalaz5._

  def number[A](t: Tree[A]): State[Int, Tree[(A, Int)]] = t match {
    case Leaf(x) =>
      for(s <- init[Int]; n <- modify((_:Int) + 1))
        yield Leaf((x, s))
    case Branch(l, r) =>
      for(lt <- number(l); rt <- number(r))
        yield Branch(lt, rt)
  }

  def main(args: Array[String]) = {
    def go(tree: Tree[Int]) {
      println(tree)
      println(number(tree) ! 10)
    }

    go(Branch(Branch(Leaf(2), Leaf(6)), Leaf(9)))
    go(Branch(Branch(Leaf(2), Branch(Leaf(4), Leaf(5))), Leaf(9)))
  }

}
