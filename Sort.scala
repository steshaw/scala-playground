//
// Adapted from https://groups.google.com/d/topic/scala-language/Qo4u2-trgJU/discussion
//

// C# code: items.orderBy(item => item.foo).thenBy(item => item.bar)

case class Person(name:String, age:Int)

val ps = List(Person("Wilma", 25), Person("Fred", 25), Person("Barnaby", 28))

println(ps sortBy {i => (i.age, i.name)})

case class Reverse[T](t: T)
def reverse[T](t:T) = Reverse(t)

implicit def ReverseOrdering[T: Ordering]: Ordering[Reverse[T]] = Ordering[T].reverse.on(_.t)

println(ps.sortBy(p => (reverse(p.age), p.name)))

{
  implicit val o = Ordering.by((p:Person) => (p.age, p.name))
  println(ps.sorted)
}

def combine[T](o1: Ordering[T], o2: Ordering[T]) = new Ordering[T] {
  def compare(x: T, y:T) =
    o1.compare(x, y) match {
      case 0 => o2.compare(x, y)
      case n => n
    }
}
def multiSort[T](xs: Seq[T])(orderings: Ordering[T]*) = xs.sorted(orderings.reduceLeft(combine))

{
  val answer1 = multiSort(ps)(Ordering.by(_.age), Ordering.by(_.name))
  println(answer1)

  val answer2 = multiSort(ps)((Ordering.by[Person, Int](_.age)).reverse, Ordering.by(_.name))
  println(answer2)
}

{
  import scala.math.Ordering.by

  val answer1 = multiSort(ps)(by(_.age), by(_.name))
  println(answer1)

  val answer2 = multiSort(ps)((by[Person, Int](_.age)).reverse, by(_.name))
  println(answer2)
}

{
  import scala.math.Ordering.by
  val ordering = combine(by[Person, Int](_.age).reverse, by[Person, String](_.name))
}

{
  import scala.math.Ordering.by
  class ThenBy[T](o: Ordering[T]) {
    def thenBy(o2: Ordering[T]): Ordering[T] = combine(o, o2)
  }
  implicit def orderingToThenBy[T](t: Ordering[T]): ThenBy[T] = new ThenBy[T](t)
  val ordering = by[Person, Int](_.age).reverse.thenBy(by[Person, String](_.name))
  val answer2 = ps.sorted(ordering)
  println(answer2)
}
