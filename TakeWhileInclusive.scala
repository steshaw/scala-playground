//
// See http://stackoverflow.com/questions/9329876/scala-extending-the-iterator
//

//
// From Rex Kerr
//
{
  import scala.language.implicitConversions

  class InclusiveIterator[A](ia: Iterator[A]) {
    def takeWhileInclusive(p: A => Boolean) = {
      var done = false
      val p2 = (a: A) => !done && { if (!p(a)) done=true; true }
      ia.takeWhile(p2)
    }
  }
  implicit def iterator_can_include[A](ia: Iterator[A]) = new InclusiveIterator(ia)

  println((0 to 10).iterator.takeWhileInclusive(_ < 4).toList)
}

//
// From Travis Brown
//
{
  import scala.language.implicitConversions

  class IteratorExtension[A](i : Iterator[A]) {
    def takeWhileInclusive(p: A => Boolean) = {
      val (a, b) = i.span(p)
      a ++ (if (b.hasNext) Some(b.next) else None)
    }
  }

  object ImplicitIterator {
    implicit def extendIterator[A](i : Iterator[A]) = new IteratorExtension(i)
  }

  import ImplicitIterator._
  println((0 to 10).iterator.takeWhileInclusive(_ < 4).toList)
}

//
// From Travis Brown + nice edit from commentor.
//
{
  import scala.language.implicitConversions

  class IteratorExtension[A](i : Iterator[A]) {
    def takeWhileInclusive(p: A => Boolean) = {
      val (a, b) = i.span(p)
      a ++ (b take 1)
    }
  }

  object ImplicitIterator {
    implicit def extendIterator[A](i : Iterator[A]) = new IteratorExtension(i)
  }

  import ImplicitIterator._
  println((0 to 10).iterator.takeWhileInclusive(_ < 4).toList)
}

//
// Additional golfing from oxbox_lakes (Chris Marshall).
//
{
  import scala.language.implicitConversions
  import scala.language.reflectiveCalls

  implicit def Pair_Is_Foldable[A, B](pair: (A, B)) = new {
    def fold[C](f: (A, B) => C): C = f.tupled(pair)
  }
  implicit def Iterator_Is_TWI[A](itr: Iterator[A]) = new {
    def takeWhileInclusive(p: A => Boolean) = itr span p fold (_ ++ _.toStream.headOption)
  }
  println((0 to 10).iterator.takeWhileInclusive(_ < 4).toList)
}

//
// Mixing it up for some final golfing + use 'implicit class'.
//
{
  implicit class Pair_Is_Foldable[A, B](pair: (A, B)) {
    def fold[C](f: (A, B) => C): C = f.tupled(pair)
  }
  implicit class Iterator_Is_TWI[A](itr: Iterator[A]) {
    def takeWhileInclusive(p: A => Boolean) = itr span p fold ((a, b) => a ++ (b take 1))
  }
  println((0 to 10).iterator.takeWhileInclusive(_ < 4).toList)
}
