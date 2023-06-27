enum Expr:
  case Constant(i: Int)
  case Add(left: Expr, right: Expr)

import Expr.*

def eval(expr: Expr): Int =
    expr match
      case Constant(i) => i
      case Add(l, r) => eval(l) + eval(r)

object HuttonsRazor:
  def main(args: Array[String]) =
    val expr = Add(Constant(1), Add(Constant(20), Constant(21)))
    println(s"${expr} = ${eval(expr)}")
