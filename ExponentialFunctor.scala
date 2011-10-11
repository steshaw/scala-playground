//
// Tony Morris mentions "Exponential Functor" here: http://www.scala-lang.org/node/2685
//

case class IntCovariantFunctor[+A](f: Int => A)
case class IntContravariantFunctor[-A](f: A => Int)
case class IntExpFunctor[A](f: A => (Int, A))
