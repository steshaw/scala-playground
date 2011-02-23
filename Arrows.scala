//
// Experiments inspired from slides at https://github.com/LucDupAtGitHub/Arrow-Calculus
//

implicit def anyToBinder[A](a: A) = new {
  def bind[B](f: A => B) = {
    f(a)
  }
}

def curry[X, Y, Z]: (((X, Y)) => Z) => (X => Y => Z) = f => x => y => f(x, y)

def uncurry[X, Y, Z]: (X => Y => Z) => (((X, Y)) => Z) = f => xy => f (xy._1) (xy._2)
