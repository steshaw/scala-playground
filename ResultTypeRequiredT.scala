//
// http://stackoverflow.com/q/31285475/482382
//

import scala.util._

class Foo {

  implicit class FoldableTry[T](tryable: Try[T]) {
    def fold[X](failure: Throwable => X)(success: T => X): X = {
      tryable match {
        case Success(result) => success(result)
        case Failure(ex) => failure(ex)
      }
    }
  }

}
