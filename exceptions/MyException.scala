// 
// Adapted from http://lizdouglass.wordpress.com/2010/12/15/scala-multiple-constructors/
//

object MyException {
  private def create(message: String): RuntimeException = new RuntimeException(message)

  private def create(message: String, throwable: Throwable): RuntimeException = new RuntimeException(message, throwable)
}

//
// Added "extends RuntimeException(exception)".
// However, this still doesn't look idea as:
//   val e = MyException("oh")
//   e.getMessage => java.lang.RuntimeException: oops
// Not quite what is expected.
//
class MyException private (exception: RuntimeException) extends RuntimeException(exception) {
  def this(message: String) = this (MyException.create(message))

  def this(message: String, throwable: Throwable) = this (MyException.create(message, throwable))
}
