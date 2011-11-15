trait Functor[F[_]] {
  def fmap[A, B](fa: F[A], f: A => B): F[B]
}
 
trait Monad[M[_]] {
  def bind[A, B](ma: M[A], f: A => M[B]): M[B]
  def pure[A](a: A): M[A]
}
 
object ObserveFunctorMonad {
  def observe[K[_]](m: Monad[K]): Functor[K] = new Functor[K] {
    def fmap[A, B](fa: K[A], f: A => B): K[B] = m.bind(fa, (a: A) => m.pure(f(a)))
  }
}
