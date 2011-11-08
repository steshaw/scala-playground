//
// Does not compile.
//

import scala.collection.immutable.Stack

class Stack2[+A] extends Stack[A] {
  override def +: (elem: A): Stack[A] = this push elem
}
