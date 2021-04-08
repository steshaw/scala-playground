//
// http://stackoverflow.com/q/31285475/482382
//

import scala.util._
import scala.unchecked

class ResultTypeRequiredT {

  implicit class FoldableTry[T](tryable: Try[T]) {
    def fold[X](failure: Throwable => X)(success: T => X): X = {
      // Tried to ignore the error with @unchecked but it does not work (probably 'cause it's not a warning).
      (tryable: @unchecked) match {
        case Success(result) => success(result)
        case Failure(ex) => failure(ex)
      }
    }
  }

}
