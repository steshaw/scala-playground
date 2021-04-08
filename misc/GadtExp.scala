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

object Exp {
  trait Eval[A] { def eval: A }
  implicit def exp2evaluator[A](e: Exp[A]) = new Eval[A] {
    def eval = Exp.eval(e)
  }
  def eval[T](t: Exp[T]): T = t match {
    case LitInt(n)     => n
    case LitBool(b)    => b
    case Succ(e)       => e.eval + 1
    case Add(e1, e2)   => e1.eval + e2.eval
    case Mul(e1, e2)   => e1.eval * e2.eval
    case IsZero(e)     => e.eval == 0
    case If(c, thenE, elseE) 
                       => eval(if (c.eval) thenE else elseE)
    case Eq(e1, e2)    => e1.eval == e2.eval
  }
}

object GadtExp extends App {
  import Exp._
  Console.println(
    If(IsZero(LitInt(1)), LitInt(41), Succ(LitInt(41))).eval)
  Console.println(
    If(Eq(LitInt(1), LitInt(0)), LitInt(41), Succ(LitInt(41))).eval)
}
