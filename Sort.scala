//
// Adapted from https://groups.google.com/d/topic/scala-language/Qo4u2-trgJU/discussion
//

// C# code: items.orderBy(item => item.foo).thenBy(item => item.bar)


object Sort {
  case class Person(name:String, age:Int)

  val ps = List(Person("Betty", 28), Person("Wilma", 25), Person("Fred", 25), Person("Barney", 28))

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
    val result1 = multiSort(ps)(Ordering.by(_.age), Ordering.by(_.name))
    println(result1)

    val result2 = multiSort(ps)((Ordering.by[Person, Int](_.age)).reverse, Ordering.by(_.name))
    println(result2)
  }

  {
    import scala.math.Ordering.by

    val result1 = multiSort(ps)(by(_.age), by(_.name))
    println(result1)

    val result2 = multiSort(ps)((by[Person, Int](_.age)).reverse, by(_.name))
    println(result2)
  }

  {
    import scala.math.Ordering.by
    val ordering = combine(by[Person, Int](_.age).reverse, by[Person, String](_.name))
  }

  {
    import scala.math.Ordering.by
    implicit def orderingToThenBy[T](t: Ordering[T]) = new {
      def thenBy(ordering: Ordering[T]): Ordering[T] = new Ordering[T] {
        def compare(a: T, b: T) =
          t.compare(a, b) match {
            case 0 => ordering.compare(a, b)
            case n => n
          }
      }
    }
    val ordering = by[Person, Int](_.age).reverse.thenBy(by[Person, String](_.name))
    val ordering2 = orderingToThenBy(by[Person, Int](_.age).reverse).thenBy(by[Person, String](_.name))
    val result2 = ps.sorted(ordering)
    println(result2)
  }

  {
    import scala.math.Ordering.by
    implicit def orderingToThenBy2[T](t: Ordering[T]) = new {
      def thenBy[S](f: (T) => S)(implicit orderingS: Ordering[S]): Ordering[T] = combine(t, Ordering.by(f)(orderingS))
    }
    val result = ps.sorted(by((p: Person) => p.age).reverse thenBy (_.name))
    println(result)
  }

  {
    import scala.math.Ordering.by
    // Provide an alias: orderBy = sorted.
    implicit def seqToOrderBy[T](t: Seq[T]) = new {
      def orderBy(ordering: Ordering[T]): Seq[T] = t.sorted(ordering)
    }
    implicit def orderingToThenBy2[T](t: Ordering[T]) = new {
      def thenBy[S](f: (T) => S)(implicit orderingS: Ordering[S]): Ordering[T] = combine(t, by(f)(orderingS))
    }
    // C# code: items.orderBy(item => item.foo).thenBy(item => item.bar)
    val result = ps.orderBy (by((p: Person) => p.age).reverse thenBy (_.name))
    println(result)
  }

  def main(args: Array[String]) {
    println("main")
  }
}
