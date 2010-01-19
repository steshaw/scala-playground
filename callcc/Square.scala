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
