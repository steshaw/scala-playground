//
// From https://groups.google.com/d/topic/scala-language/wrnflKVxw0I/discussion
//
trait A[a]
case class B[a] extends A[a]

class C[a] {
  def f(): A[a] = {
    def g(x: A[a]): A[a] = x match {
      case y:B[a] => {
        g(y)
      }
    }
      
    g(null)
  }
}
