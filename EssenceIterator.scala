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

def intPlusMonoid = new Monoid[Int] {
  override def zero = 0
  override def append(a: Int, b: Int) = a + b
}
def intTimesMonoid = new Monoid[Int] {
  override def zero = 1
  override def append(a: Int, b: Int) = a * b
}

def sum(list: List[Int]): Int = accumulate(list)(intPlusMonoid)
def product(list: List[Int]): Int = accumulate(list)(intTimesMonoid)
