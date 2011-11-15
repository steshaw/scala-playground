//
// From http://blog.tmorris.net/lifting/
//

trait Lift[F[_]] {
  // Spot the pattern in these type signatures
  // of increasing arity.
 
  def lift0[A]:
    A => F[A]
 
  def lift1[A, B]:
    (A => B) => (F[A] => F[B])
 
  def lift2[A, B, C]:
    (A => B => C) => (F[A] => F[B] => F[C])
 
  def lift3[A, B, C, D]:
    (A => B => C => D) => (F[A] => F[B] => F[C] => F[D])
 
  // ... and so on
 
  // The relationship between lift<N> and lift<N-1>
  // can be given by a function,
 
  def ap[A, B]:
    F[A => B] => (F[A] => F[B])
}
 
trait LiftImpl[F[_]] extends Lift[F] {
  // Each lift function uses
  // the previous lift function and ap.
 
  def lift1[A, B]:
    (A => B) => (F[A] => F[B])
    = ap compose lift0
 
  def lift2[A, B, C]:
    (A => B => C) => (F[A] => F[B] => F[C])
    = f => ap compose lift1(f)
 
  def lift3[A, B, C, D]:
    (A => B => C => D) => (F[A] => F[B] => F[C] => F[D])
    = f => a => ap compose lift2(f)(a)
}
 
/*
// Notes
// * lift0 is often called: unit, return, pure, point, η
// * lift1 is often called: fmap, map, ∘
// * lift<N> is often called: liftA<N>, liftM<N>
All that is left to do is to implement the LiftImpl trait! You can do this by implementing the ap and lift0 functions.

Examples of implementations that I know will work out if you try to implement them:

class ListLift extends LiftImpl[List]
class OptionLift extends LiftImpl[Option]
class EitherLift[R] extends LiftImpl[({type λ[α] = Either[R, α]})#λ]
class Function1Lift[R] extends LiftImpl[({type λ[α] = R => α})#λ]
Those last couple are a bit funky, but a lot of that is syntax noise rather than anything too complicated. Fill out the body of those classes!
*/
