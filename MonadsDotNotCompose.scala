//
// From http://blog.tmorris.net/monads-do-not-compose/
//

trait Functor[F[_]] {
  def fmap[A, B](f: A => B, a: F[A]): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def ap[A, B](f: F[A => B], a: F[A]): F[B]
  def point[A](a: A): F[A]
  override final def fmap[A, B](f: A => B, a: F[A]) =
    ap(point(f), a)
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](f: A => F[B], a: F[A]): F[B]
  override final def ap[A, B](f: F[A => B], a: F[A]) =
    flatMap((ff: A => B) => fmap((aa: A) => ff(aa), a), f)
}

object Compose {
  def FunctorCompose[M[_], N[_]](implicit mf: Functor[M], nf: Functor[N]): Functor[({type λ[α]=M[N[α]]})#λ] =
    new Functor[({type λ[α]=M[N[α]]})#λ] {
      def fmap[A, B](f: A => B, a: M[N[A]]) = mf.fmap((na: N[A]) => nf.fmap(f, na), a)
    }

  def ApplicativeCompose[M[_], N[_]](implicit ma: Applicative[M], na: Applicative[N]): Applicative[({type λ[α]=M[N[α]]})#λ] =
    new Applicative[({type λ[α]=M[N[α]]})#λ] {
      def ap[A, B](f: M[N[A => B]], a: M[N[A]]) = {
        def liftA2[X, Y, Z](f: X => Y => Z, a: M[X], b: M[Y]): M[Z] =
          ma.ap(ma.fmap(f, a), b)
        liftA2((ff: N[A => B]) => (aa: N[A]) => na.ap(ff, aa), f, a)
      }
      def point[A](a: A) =
        ma point (na point a)
    }

  def MonadCompose[M[_], N[_]](implicit mm: Monad[M], nm: Monad[N]): Monad[({type λ[α]=M[N[α]]})#λ] = sys.error("uninhabited")
}
