//
// Anonymous functions defined with case (pattern-matching) for functions with arity
// greater than 1. Introduced in Scala 2.5 (02-May-2007).
//
// Adapted from http://www.scala-lang.org/node/43
//
// "It is now possible to use case clauses to define a function value directly for
// functions of arities greater than one. Previously, only unary functions could be
// defined that way."
//

def dotProduct(xs: Array[Double], ys: Array[Double]) =
  (0.0 /: (xs zip ys)) {
    case (a, (b, c)) => a + b * c
  }

val acc1 = (a: Double, p: (Double, Double)) => p match { case (b, c) => a + b * c }
//val acc2 = (a: Double, p: (Double, Double)) => { case (a, (b, c)) => a + b * c } // Does not compile.

println(dotProduct(Array(1.0, 2.0, 3.0), Array(3.0, 2, 1)))
