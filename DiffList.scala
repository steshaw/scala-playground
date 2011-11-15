//
// From http://blog.tmorris.net/list-with-o1-cons-and-snoc-in-scala/
//

sealed trait DiffList[A] {
  val endo: List[A] => List[A]
 
  import DiffList._
 
  // cons O(1)
  def <::(a: A): DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => a :: DiffList.this.endo(z)
  }
 
  // snoc O(1)
  def ::>(a: A): DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => DiffList.this.endo(a :: z)
  }
 
  // append O(1)
  def :::>(a: DiffList[A]): DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => DiffList.this.endo(a.endo(z))
  }
 
  // O(n)
  def toList = endo(Nil)
}
 
object DiffList {
  def empty[A]: DiffList[A] = new DiffList[A] {
    val endo = (z: List[A]) => z
  }
 
  def single[A](a: A): DiffList[A] = a <:: empty[A]
}
