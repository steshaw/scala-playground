import scala.collection.immutable.Stack
import scala.collection.generic.CanBuildFrom

class Stack2[+A] extends Stack[A] {
  // Trying to override the less efficient prepend with the efficient 'push' for Stack but
  // the types don't match...
  override def +: [B >: A, That] (elem: B)(implicit bf: CanBuildFrom[Stack[A], B, That]): That =
    this push elem // FIXME: compile error
}
