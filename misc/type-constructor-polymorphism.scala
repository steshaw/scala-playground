//
// See http://debasishg.blogspot.com.au/2009/01/higher-order-abstractions-in-scala-with.html
//

val employees: List[Option[String]] =
  List(Some("dave"), None, Some("john"), Some("sam"))

val n =
  employees.map { x =>
    x match {
      case Some(name) => name.length
      case None => 0
    }
  }.reduceLeft[Int](_ + _)
println(n)

val brs: List[List[String]] =
  List(List("dave", "john", "sam"), List("peter", "robin", "david"), List("pras", "srim"))

val m =
  brs.flatMap {x => x.map {_.length}}
      .reduceLeft[Int](_ + _)
println(m)

val o = (for (group <- brs; person <- group) yield person.length).reduceLeft[Int](_ + _)
println(o)

def flatMapTo[U,C[X]](f: T ⇒ Iterable[U])
                     (b: Buildable[C]): C[U] = {
  val buff = b.build[U]
  val elems = elements
  while (elems.hasNext)
    f(elems.next).elements.foreach{ el ⇒ buff += el }
  buff.finalise()
}

val l: List[Int] = employees.flatMapTo[Int, List]{_.map{_.length}}
val sum: Int = l.elements.reduceLeft[Int](_ + _)
println(sum)
