//
// See http://dl.dropbox.com/u/5930402/EssenceIteratorPattern.odp
//

trait Accumulator[T] {
  def zero: T
  def append[T](a1: T, a2: T): T
}

def accumulate[A: Accumulator](list: List[A]): A = {
  val acc = implicitly[Accumulator[A]]
  var total = acc.zero
  for (a <- list) {
    total = acc.append(total, a)
  }
  total
}
