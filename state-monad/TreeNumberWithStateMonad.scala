package steshaw

object TreeNumberWithStateMonad {

  sealed abstract class Tree[+A]
  final case class Leaf[A](a: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // Failed attempt to implement the state monad thingo starting with Tony's code from the blog post.

  trait State[S, A] {
    val s: Function[S, (S, A)]
    def flatMap[B](f: A => Function[S, (S, B)]): Function[S, (S, B)] = x => s(x) match {
        case (s, a) => f(a)(s)
    }
    def map[B](f: A => B): Function[S, (S, B)] = s0 => s(s0) match {
      case (s, a) => (s, f(a))
    }
  }

/*
  def mkState[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
    def apply(s: S) = f(s)
  }
*/

  def init[S] = new State[S, S] {
    override val s: Function[S, (S, S)] = (init => (init, init))
  }

  def modify[S](f: S => S) = new State[S, S] {
    override val s: Function[S, (S, S)] = (init => (f(init), init))
  }

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
