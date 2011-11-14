//
// From http://blog.tmorris.net/monad-exercises-in-scala/
//

import sys.error

// 1. Start here. Observe this trait
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
  // 2. Replace error("todo") with an implementation
  def ListMonad: Monad[List] = error("todo")
 
  // 3. Replace error("todo") with an implementation
  def OptionMonad: Monad[Option] = error("todo")
 
  // 4. Replace error("todo") with an implementation
  def InterMonad: Monad[Inter] = error("todo")
 
  // 5. Replace error("todo") with an implementation
  def IdentityMonad: Monad[Identity] = error("todo")
}
 
object MonadicFunctions {
  // 6. Replace error("todo") with an implementation
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] =
    error("todo")
 
  // 7. Replace error("todo") with an implementation
  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] =
    error("todo")
 
  // 8. Replace error("todo") with an implementation
  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] =
    error("todo")
 
  // 9. Replace error("todo") with an implementation
  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] =
    error("todo")
 
  // 10. Replace error("todo") with an implementation
  def filterM[M[_], A](f: A => M[Boolean], as: List[A]
    , m: Monad[M]): M[List[A]] =
    error("todo")
 
  // 11. Replace error("todo") with an implementation
  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] =
    error("todo: flatMap n times to produce a list")
 
  // 12. Replace error("todo") with an implementation
  def lift2[M[_], A, B, C](f: (A, B) => C, a: M[A], b: M[B]
    , m: Monad[M]): M[C] =
    error("todo")
 
  // lift3, lift4, etc. Interesting question: Can we have liftN?
}
