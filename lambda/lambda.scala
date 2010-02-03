//
// See p63-64 Scala Reference 2.7 Draft 15-Mar-2009
//

class Expr
case class Var(x: String) extends Expr
case class Apply(f: Expr, e: Expr) extends Expr
case class Lambda(x: String, e: Expr) extends Expr
case class Number(x: Int) extends Expr

object lambda {
  type Env = String => Value
  case class Value(e: Expr, env: Env)

  def eval(e: Expr, env: Env): Value = e match {
    case Var(x) => env(x)
    case Apply(f, g) =>
      val Value(Lambda (x, e1), env1) = eval(f, env)
      val v = eval(g, env)
      eval(e1, (y => if (y == x) v else env1(y)))
    case Lambda(_, _) => Value(e, env)
  }

}
