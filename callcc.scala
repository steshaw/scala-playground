//
// See http://blog.tmorris.net/continuation-monad-in-scala/
//

sealed trait Continuation[R, +A] {
  def apply(f: A => R): R
 
  import Continuation.continuation
 
  def map[B](k: A => B) = 
    continuation[R, B](z => apply(z compose k))
 
  def flatMap[B](k: A => Continuation[R, B]) = 
    continuation[R, B](z => apply(k(_)(z)))
}

object Continuation {
  def continuation[R, A](g: (A => R) => R) = new Continuation[R, A] {
    def apply(f: A => R) = g(f)
  }
 
  def unit[R] = new {
    def apply[A](a: A) = continuation[R, A](f => f(a))
  }
 
  def callcc[R, A, B](f: (A => Continuation[R, B]) => Continuation[R, A]) =
    continuation[R, A](k => f(a => continuation(x => k(a)))(k))
}

object Square {
  import Continuation._
 
  def square(n: Int) = n * n
 
  // Continuation for square
  def squarec[R](n: Int) = unit[R](square(n))
 
  // Continuation for effect (Unit) on square.
  // This is simply to help the type inferencer by applying a type argument.
  def squareE(n: Int) = squarec[Unit](n)
 
  def main(args: Array[String]) {
    val k = squareE(args(0).toInt)
    k(println)
  }
}

object Divide {
  import Continuation._
 
  // Division
  def div[R](c: String => Continuation[R, Int])
            (n: Int, // numerator
             d: Int) // denominator
                    : Continuation[R, Int] =
    callcc[R, Int, String](ok => 
      callcc[R, String, String](err =>
        if(d == 0) err("Denominator 0")
        else ok(n / d)
      ) flatMap c)
 
  def main(args: Array[String]) {
    def divError[R] = div[R](sys.error(_)) _
    println(divError(7, 3)(x => x))
    println(divError(7, 0)(x => x)) // throws error
  }
}

object Scheme1 extends App {
  // Trying to simulate the Scheme expression: 
  //
  // (+ 1 (call/cc
  //         (lambda (k)
  //             (+ 2 (k 4)))))
  //
  import Continuation._

  def onePlus[R] = unit[R](1 + (_: Int))
  def twoPlus[R] = unit[R](2 + (_: Int))
  //val r = add(f => f(2))
/*
  val r = onePlus(
      callcc((k: Continuation[Int, Int]) => 
        twoPlus(k((a: Int) => 4))))
*/

  {
    val r = onePlus(f => f(2))
    println(r)
  }

  {
    val r = twoPlus(f => f(3))
    println(r)
  }

  {
    val r = onePlus(f => f(twoPlus(f => f(10))))
    println(r)
  }

  {
    val r = callcc[Int, Int, Int](a => 
      callcc[Int, Int, Int](b =>
        b(4)))
/*
    val r: Continuation[Int, Int] = onePlus(
        callcc[Int, Int, Int](k =>
          (b: Int) => twoPlus(k)))
*/
    println(r(a => a))
  }
}
