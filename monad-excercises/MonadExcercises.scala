//
// From http://blog.tmorris.net/monad-exercises-in-scala/
//

import sys.error

trait Monad[M[_]] {
  def flatMap[A, B](a: M[A], f: A => M[B]): M[B]
  def unital[A](a: A): M[A]
}
 
// A simple data type, which turns out to satisfy the above trait
case class Inter[A](f: Int => A)
 
// So does this.
case class Identity[A](a: A)
 
// Monad implementations
object Monad {
  def ListMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](a: List[A], f: A => List[B]): List[B] = a flatMap f
    override def unital[A](a: A): List[A] = List(a)
  }
 
  def OptionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](a: Option[A], f: A => Option[B]): Option[B] = a flatMap f
    override def unital[A](a: A): Option[A] = Option(a)
  }
 
  def InterMonad: Monad[Inter] = new Monad[Inter] {
    override def flatMap[A, B](a: Inter[A], f: A => Inter[B]): Inter[B] = Inter((n: Int) => f(a.f(n)).f(n))
    override def unital[A](a: A): Inter[A] = Inter((n) => a)
  }
 
  def IdentityMonad: Monad[Identity] = new Monad[Identity] {
    override def flatMap[A, B](a: Identity[A], f: A => Identity[B]): Identity[B] = f(a.a)
    override def unital[A](a: A): Identity[A] = Identity(a)
  }
}
 
object MonadicFunctions {
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] = as match {
    case Nil => m.unital(Nil)
    case ma :: mas => {
      var recurse = sequence(mas, m)
      m.flatMap(ma, (a: A) => m.flatMap(recurse, (as: List[A]) => m.unital(a :: as)))
    }
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Option(Nil)
    case oa :: as => {
      var recurse = sequence(as)
      oa.flatMap((a: A) => recurse.flatMap((as: List[A]) => Option(a :: as)))
    }
  }

  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] =
    m.flatMap(a, (a: A) => m.unital(f(a)))
 
  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] =
    m.flatMap(a, identity[M[A]])
 
  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] =
    m.flatMap(f, (f: A => B) => m.flatMap(a, (a: A) => m.unital(f(a))))
 
  def filterM[M[_], A](f: A => M[Boolean], as: List[A], m: Monad[M]): M[List[A]] = as match {
    case Nil     => m.unital(Nil)
    case a :: as => {
      var mb = f(a)
      var rest = filterM(f, as, m)
      m.flatMap(mb, (b: Boolean) => m.flatMap(rest, (as: List[A]) => m.unital(if (b) a :: as else as)))
    }
  }
 
  def replicateM[M[_], A](n: Int, ma: M[A], m: Monad[M]): M[List[A]] =
    if (n <= 0) m.unital(Nil)
    else {
      var rest = replicateM(n-1, ma, m)
      m.flatMap(ma, (a: A) => m.flatMap(rest, (as: List[A]) => m.unital(a :: as)))
    }
 
  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B], m: Monad[M]): M[C] =
    m.flatMap(a, (a: A) => m.flatMap(b, (b: B) => m.unital(f(a, b))))
 
  // lift3, lift4, etc. Interesting question: Can we have liftN?
}
