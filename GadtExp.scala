//
// Adapted from example in the Scala 2.8 distribution.
//         and http://lambdalog.seanseefried.com/posts/2011-11-22-gadts-in-scala.html
//

  abstract class Exp[T]
  case class LitInt(i: Int)                  extends Exp[Int]
  case class LitBool(b: Boolean)             extends Exp[Boolean]
  case class Succ(t: Exp[Int])               extends Exp[Int]
  case class Add(e1: Exp[Int], e2: Exp[Int]) extends Exp[Int]
  case class Mul(e1: Exp[Int], e2: Exp[Int]) extends Exp[Int]
  case class IsZero(t: Exp[Int])             extends Exp[Boolean]
  case class If[T](c: Exp[Boolean],
                   thenE: Exp[T],
                   elseE: Exp[T])            extends Exp[T]
  case class Eq[A](e1: Exp[A], e2: Exp[A])   extends Exp[Boolean]

object GadtExp extends App {

  def eval[T](t: Exp[T]): T = t match {
    case LitInt(n)     => n
    case LitBool(b)    => b
    case Succ(u)       => eval(u) + 1
    case Add(e1, e2)   => eval(e1) + eval(e2)
    case Mul(e1, e2)   => eval(e1) * eval(e2)
    case IsZero(u)     => eval(u) == 0
    case If(c, thenE, elseE) 
                       => eval(if (eval(c)) thenE else elseE)
    case Eq(e1, e2)    => eval(e1) == eval(e2)
  }
  Console.println(
    eval(If(IsZero(LitInt(1)), LitInt(41), Succ(LitInt(41)))))
  Console.println(
    eval(If(Eq(LitInt(1), LitInt(0)), LitInt(41), Succ(LitInt(41)))))
}
