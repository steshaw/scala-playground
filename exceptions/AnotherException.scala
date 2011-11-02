// 
// Adapted from http://stackoverflow.com/questions/3299776/in-scala-how-can-i-subclass-a-java-class-with-multiple-constructors/3299832#3299832
//
// This is a pretty good solution. The toString method is still a bit surprising:
//
//   scala> val e = AnotherException("oh")
//   e: java.lang.RuntimeException with AnotherException = AnotherException$$anon$1: oh
//   
//   scala> e.toString
//   res0: java.lang.String = AnotherException$$anon$1: oh
//

trait AnotherException extends RuntimeException

object AnotherException {
  def apply(message: String) = new RuntimeException(message) with AnotherException
  def apply(message: String, cause: Throwable) = new RuntimeException(message, cause) with AnotherException
}
