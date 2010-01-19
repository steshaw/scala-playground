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
    def divError[R] = div[R](error(_)) _
    println(divError(7, 3)(x => x))
    println(divError(7, 0)(x => x)) // throws error
  }

}
