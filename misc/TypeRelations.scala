//
// Found at http://gist.github.com/229756
//
// See scala mailing list discussion http://scala-programming-language.1934581.n4.nabble.com/scala-Using-generalised-type-constraints-in-2-8-collections-td2004010.html
//

// Named <::< to clearly distinguish from Predef.<:<
sealed abstract class <::<[-From, +To] extends (From => To)
object <::< {
  implicit def conforms[A]: A <::< A = new (A <::< A) {def apply(x: A) = x}
}

sealed abstract class =:=[From, To] extends (From => To)
object =:= {
  implicit def tpEquals[A]: A =:= A = new (A =:= A) {def apply(x: A) = x}
}

sealed abstract class <%<[-From, +To] extends (From => To)
object <%< {
  implicit def conformsOrViewsAs[A <% B, B]: A <%< B = new (A <%< B) {def apply(x: A) = x}
}

trait A
trait B
implicit def AToB(a: A): B = new B {}

println("(implicitly[Int =:= Int], implicitly[Int <::< Any], implicitly[A <%< B]) = " + 
         (implicitly[Int =:= Int], implicitly[Int <::< Any], implicitly[A <%< B]))

//object Predef {
  implicit def identity[A](a: A): A = a
//}

println("(implicitly[Int => Int], implicitly[Int => Any], implicitly[A => B]) = " +
         (implicitly[Int => Int], implicitly[Int => Any], implicitly[A => B]))

// Isomorphism between A => B and A <%< B
println("(implicitly[A => B], implicitly[A <%< B]) = " +
         (implicitly[A => B], implicitly[A <%< B]))

// Compile Errors:
// implicitly[Int =:= Any] // error: could not find implicit value for parameter e: this.=:=[Int,Any]
// implicitly[A <::< B] // error: could not find implicit value for parameter e: this.<::<[this.A,this.B]
