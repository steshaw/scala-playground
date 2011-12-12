//
// See http://dl.dropbox.com/u/5930402/EssenceIteratorPattern.odp
//     https://groups.google.com/d/msg/scala-melb/5VbJmfNK_h0/Cysnk1hUjoUJ
//

trait Monoid[T] {
  def zero: T
  def append(a1: T, a2: T): T
}

def accumulate[A: Monoid](list: List[A]): A = {
  val acc = implicitly[Monoid[A]]
  var total = acc.zero
  for (a <- list) {
    total = acc.append(total, a)
  }
  total
}

val intPlusMonoid = new Monoid[Int] {
  override def zero = 0
  override def append(a: Int, b: Int) = a + b
}
val intTimesMonoid = new Monoid[Int] {
  override def zero = 1
  override def append(a: Int, b: Int) = a * b
}
implicit val stringMonoid = new Monoid[String] {
  def zero = ""
  def append(a: String, b: String) = a + b
}
implicit def listMonoid[A] = new Monoid[List[A]] {
  def zero = List()
  def append(a: List[A], b: List[A]) = a ++ b
}

def sum(list: List[Int]): Int = accumulate(list)(intPlusMonoid)
def product(list: List[Int]): Int = accumulate(list)(intTimesMonoid)
def appendAll(list: List[String]) = accumulate(list)
def flatten[A](list: List[List[A]]) = accumulate(list)

val booleanAndMonoid = new Monoid[Boolean] {
  def zero = true
  def append(a: Boolean, b: Boolean) = a && b
}
val booleanOrMonoid = new Monoid[Boolean] {
  def zero = false
  def append(a: Boolean, b: Boolean) = a || b
}
def all(list: List[Boolean]) = accumulate(list)(booleanAndMonoid)
def any(list: List[Boolean]) = accumulate(list)(booleanOrMonoid)
