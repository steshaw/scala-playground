// 
// Adapted from http://lizdouglass.wordpress.com/2010/12/15/scala-multiple-constructors/
//

object MyException {
  private def create(message: String): RuntimeException = new RuntimeException(message)

  private def create(message: String, throwable: Throwable): RuntimeException = new RuntimeException(message, throwable)
}

// Unfortunately this isn't an exception and cannot be used in a throw expression.
class MyException private (exception: RuntimeException) {
  def this(message: String) = this (MyException.create(message))

  def this(message: String, throwable: Throwable) = this (MyException.create(message, throwable))
}
