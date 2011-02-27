//
// Adapted from https://groups.google.com/d/topic/scala-language/Qo4u2-trgJU/discussion
//

// C# code: items.orderBy(item => item.foo).thenBy(item => item.bar)


object Sort {
  case class Person(name:String, age:Int)

  val ps = List(
      Person("Barney", 24),
      Person("Wilma", 25), 
      Person("Betty", 24), 
      Person("Fred", 25)
  )

  println(ps sortBy {i => (i.age, i.name)})
  println(ps sortBy {i => (-i.age, i.name)})
  println()

  {
    case class Reverse[T](t: T)
    def reverse[T](t:T) = Reverse(t)

    implicit def ReverseOrdering[T: Ordering]: Ordering[Reverse[T]] = Ordering[T].reverse.on(_.t)

    {
      implicit val o = Ordering.by((p:Person) => (p.age, p.name))
      println(ps.sorted)
    }

    println(ps.sortBy(p => (reverse(p.age), p.name)))

    println()
  }

  def combine[T](o1: Ordering[T], o2: Ordering[T]) = new Ordering[T] {
    def compare(x: T, y:T) =
      o1.compare(x, y) match {
        case 0 => o2.compare(x, y)
        case n => n
      }
  }

  {
    def multiSort[T](xs: Seq[T])(orderings: Ordering[T]*) = {
      xs.sorted(orderings.reduceLeft(combine))
    }

    import scala.math.Ordering.by

    val result1 = multiSort(ps)(by(_.age), by(_.name))
    println(result1)

    val result2 = multiSort(ps)((by[Person, Int](_.age)).reverse, by(_.name))
    println(result2)

    println()
  }

  {
    import scala.math.Ordering.by
    {
      val ordering = combine(by[Person, Int](_.age), by[Person, String](_.name))
      val result = ps.sorted(ordering)
      println(result)
    }
    {
      val ordering = combine(by[Person, Int](_.age).reverse, by[Person, String](_.name))
      val result = ps.sorted(ordering)
      println(result)
    }

    println()
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
    {
      val ordering = by[Person, Int](_.age).thenBy(by[Person, String](_.name))
      val result1 = ps.sorted(ordering)
      println(result1)
    }
    {
      val ordering = by[Person, Int](_.age).reverse.thenBy(by[Person, String](_.name))
      val result2 = ps.sorted(ordering)
      println(result2)
    }

    println()
  }

  {
    import scala.math.Ordering.by
    implicit def orderingToThenBy2[T](t: Ordering[T]) = new {
      def thenBy[S](f: (T) => S)(implicit orderingS: Ordering[S]): Ordering[T] = combine(t, Ordering.by(f)(orderingS))
    }
    val result1 = ps.sorted(by((p: Person) => p.age) thenBy (_.name))
    println(result1)
    val result2 = ps.sorted(by((p: Person) => p.age).reverse thenBy (_.name))
    println(result2)

    println()
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
    implicit def functionToThenBy[A, B](f1: A=>B)(implicit orderingB: Ordering[B]) = new {
      def thenBy[S](f2: (A) => S)(implicit orderingS: Ordering[S]): Ordering[A] = combine(by(f1)(orderingB), by(f2)(orderingS))
    }
    implicit def functionToOrdering[A, B](f: A=>B)(implicit orderingB: Ordering[B]): Ordering[A] = Ordering.by(f)

    // C# code: items.orderBy(item => item.foo).thenBy(item => item.bar)
    val result1 = ps.orderBy (((p: Person) => p.age) thenBy (_.name))
    println(result1)

    val result2 = ps.orderBy (((p: Person) => p.age).reverse thenBy (_.name))
    println(result2)

    println()
  }

  def main(args: Array[String]) {
    println("main")
  }
}
