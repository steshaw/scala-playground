//
// FIX: need scalaz
//

object OverConstrainedCode extends App {
  import scalaz._, Scalaz._
 
  // here is a simplified case of what commonly happens
 
  case class Good[F[_], A](run: Int => F[A]) {
    def map[B](f: A => B)(implicit F: Functor[F]): Good[F, B] =
      Good(i => run(i).map(f))
 
    def flatMap[B](f: A => Good[F, B])(implicit F: Monad[F]): Good[F, B] =
      Good(i => run(i).flatMap(a => f(a).run(i)))
  }
 
  case class Bad[F[_]: Monad, A](run: Int => F[A]) {
    def map[B](f: A => B): Bad[F, B] =
      Bad(i => run(i).map(f))
 
    def flatMap[B](f: A => Bad[F, B]): Bad[F, B] =
      Bad(i => run(i).flatMap(a => f(a).run(i)))
  }
 
 
  // Option is all good to flat map, yay!
  Good[Option, Int](Some.apply).flatMap(n =>
    if (n > 7) Good[Option, Int](Some.apply) else Good[Option, Int](_ => None))
 
  // Validation isn't a Monad but I can still map, yay!
  type ValidationX[A] = ValidationNel[String, A]
  Good[ValidationX, Int](_.successNel).map(_ + 1)
 
 
  // Over constrained =====
 
 
  // Option is all good to flat map, yay!
  Bad[Option, Int](Some.apply).flatMap(n =>
    if (n > 7) Bad[Option, Int](Some.apply) else Bad[Option, Int](_ => None))
 
  // Validation isn't a Monad but I can't construct to map over :(
// doesn't compile
//  Bad[ValidationX, Int](_.successNel).map(_ + 1)
 
}
